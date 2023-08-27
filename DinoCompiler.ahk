﻿; General DinoCode compiler .config --> .exe
; By @BlassGO
;
#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#SingleInstance Ignore
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
SetBatchLines, -1
#include bin\dinocode\Crypt.ahk

; AHK2 configs
;@Ahk2Exe-SetName         DinoCode Compiler
;@Ahk2Exe-SetDescription  .config to .exe
;@Ahk2Exe-SetCopyright    Copyright (c) since 2023
;@Ahk2Exe-SetCompanyName  by BlassGO
;@Ahk2Exe-SetMainIcon bin\icon.ico
;@Ahk2Exe-AddResource bin\icon.ico, 160
;@Ahk2Exe-AddResource bin\icon.ico, 206
;@Ahk2Exe-AddResource bin\icon.ico, 207
;@Ahk2Exe-AddResource bin\icon.ico, 208

; Dependencies path
current := A_ScriptDir "\bin"
global Delimiter := Chr(34)
      , Escape := "\"
      , ahk2exe := current "\Ahk2Exe.exe"
	   , bin := current "\Unicode 64-bit.bin"
	   , dinocode := current "\dinocode"
      , tmp := dinocode "\build.tmp"
	   , compress
      , out
	   , config
      , libs, extralibs, error, with_gui, admin
	   , icon := current "\icon.ico"
	   , SetName          := "DinoCode"
	   , SetDescription   := "Compiled DinoScript"
	   , SetCopyright     := "Copyright (c) since 2023"
	   , SetCompanyName   := "DinoCode"
	   , SetOrigFilename

; Check
StringCaseSense, Off
Files := ahk2exe "," bin
Loop, parse, Files, `,
   (A_LoopField && !InStr(FileExist(A_LoopField), "A")) ? abort("Cant find """ basename(A_LoopField) """ file")

; Extra props
GuiFont := GuiDefaultFont()
textz := GuiFont.Size
style := GuiFont.Name
OnError("RuntimeError")
OnExit, GuiClose

; Parse ARGS
if A_Args.MaxIndex() {
   while A_Args.MaxIndex()
	   (SubStr(p1:=A_Args.RemoveAt(1),1,1)="/"&&(param:=Func("Arg_" SubStr(p1,2)))) ? ((((p2:=A_Args.RemoveAt(1))||p2!="")&&SubStr(p2,1,1)!="/") ? %param%(p2) : abort("Blank parameter-->" p1) ) : abort("Unrecognised parameter-->" p1)
} else {
  with_gui()
}
if config {
   build()
} else {
   Loop, %A_ScriptDir%/*.config
   {
     Arg_in(A_LoopFileName), Arg_out(A_LoopFileDir "\" simplename(A_LoopFileName) ".exe"), Arg_OrigFilename(A_LoopFileName)
     build()
     if error
        ExitApp
   }
   (!config) ? abort("No .config file found")
}

ExitApp

; ARGS Functions
Arg_in(str) {
   InStr(FileExist(str), "A") ? config:=str : abort("Cant find config-->" str)
}
Arg_bin(str) {
   InStr(FileExist(str), "A") ? bin:=str : abort("Cant find bin-->" str)
}
Arg_ahk2exe(str) {
   InStr(FileExist(str), "A") ? ahk2exe:=str : abort("Cant find Ahk2Exe-->" str)
}
Arg_icon(str) {
   InStr(FileExist(str), "A") ? icon:=str : abort("Cant find icon-->" str)
}
Arg_dinocode(str) {
   InStr(FileExist(str), "D") ? (dinocode:=str, tmp:=dinocode "\build.tmp") : abort("Cant find DinoCode source-->" str)
}
Arg_compress(str) {
   if str is integer
     (str>0) ? compress:=str : compress:=false
   else
     abort("Invalid compression id-->" str)
}
Arg_show(str) {
   (str=1) ? with_gui()
}
Arg_admin(str) {
   (str=1) ? admin:=true : admin:=false
}
Arg_out(str) {
   out:=str
}
Arg_Name(str) {
   SetName:=str
}
Arg_Description(str) {
   SetDescription:=str
}
Arg_Copyright(str) {
   SetCopyright:=str
}
Arg_CompanyName(str) {
   SetCompanyName:=str
}
Arg_OrigFilename(str) {
   SetOrigFilename:=str
}

; Functions
abort(str) {
   MsgBox, 262160, Build Exception, % str
   error:=true
   if !with_gui
     ExitApp
}
RuntimeError(error) {
   MsgBox, 262160, Runtime Exception, % "Internal error in -> [" . A_ScriptName . "]`n`nFile: " . RegExReplace(error.file, ".*\\([^\\]+)$", "$1") . "`nLine: " . error.line . "`nReason: " .  error.message . "`n`n---> " . error.what
   ExitApp
}
GuiDefaultFont() {
   VarSetCapacity(LF, szLF := 28 + (A_IsUnicode ? 64 : 32), 0) ; LOGFONT structure
   If DllCall("GetObject", "Ptr", DllCall("GetStockObject", "Int", 17, "Ptr"), "Int", szLF, "Ptr", &LF)
      Return {Name: StrGet(&LF + 28, 32), Size: Round(Abs(NumGet(LF, 0, "Int")) * (72 / A_ScreenDPI), 1)
            , Weight: NumGet(LF, 16, "Int"), Quality: NumGet(LF, 26, "UChar")}
   return False
}
GetFullPathName(path) {
   cc := DllCall("GetFullPathName", "str", path, "uint", 0, "ptr", 0, "ptr", 0, "uint")
   VarSetCapacity(buf, cc*(A_IsUnicode?2:1))
   DllCall("GetFullPathName", "str", path, "uint", cc, "str", buf, "ptr", 0, "uint")
   return buf
}
basename(str, fullpath := true) {
   if fullpath && FileExist(str) {
      str := GetFullPathName(str)
      SplitPath, str, Name
   } else {
      RegExMatch(str, "(.*[\\/])*\K(.+)$", Name)
   }
   return Name
}
dirname(str, fullpath := true) {
   if fullpath && FileExist(str) {
      str := GetFullPathName(str)
      SplitPath, str,, Dir
   } else {
      RegExMatch(str, "(.*?)(?=[\\/][^\\/]*$)", Dir)
   }
   return Dir
}
extname(str, fullpath := true) {
   if fullpath && FileExist(str) {
      str := GetFullPathName(str)
      SplitPath, str,,,Ext
   } else {
      RegExMatch(str, ".*\.\K(.+)$", Ext)
   }
   return Ext
}
simplename(str, fullpath := true) {
   if fullpath && FileExist(str) {
      str := GetFullPathName(str)
      SplitPath, str,,,,Name,
   } else {
      RegExMatch(str, "(.*[\\/])*\K(.*?)(?=(\.[^.]*)$)", Name)
   }
   return Name
}
solve_escape(Byref str,Byref from:="",key:="&") {
   _pos:=1,_extra:=0,_rex:="\[\" . key . "_(\d+)_\" . key . "\]"
   while,(_pos:=RegExMatch(str,_rex,_char,_pos+_extra))
      str:=RegExReplace(str,"s).{" . StrLen(_char) . "}",from[_char1],,1,_pos),_extra:=StrLen(from[_char1])
   return str
}
trim_all(str){
   return RegexReplace(str,"s)^\s*(.*?)\s*$","$1")
}
with_indent(str){
   return RegexMatch(str,"s)^(\s*)(.*?)\s*$",line_indent) ? StrLen(line_indent1) . Chr(1) . line_indent2  : ""
}
with_expr(str){
   static regex_expr
   (!regex_expr) ? (regex_expr:="\$\(((?:[^\" . Delimiter . "\(\)]+|([\" . Delimiter . "]).*?\2|\(([^\(\)]+|(?1))*\)|(?R))+)\)")
   _pos:=1, _extra:=0
   while,(_pos:=RegExMatch(str,regex_expr,_char,_pos+_extra))
       _char1:=with_indent(with_expr(_char1)), str:=RegExReplace(str,".{" . StrLen(_char)-3 . "}",_char1,,1,_pos+2), _extra:=StrLen(_char1)+3
   return str
}
read_file(file) {
   static regex_main,regex_expr
   (!regex_main) ? (regex_expr:="\$\(((?:[^\" . Delimiter . "\(\)]+|([\" . Delimiter . "]).*?\2|\(([^\(\)]+|(?1))*\)|(?R))+)\)", regex_main:="s)([^;\s]+|;\s*[^;\s]+)\s*((?:\" . Delimiter . "[\s\S]*?\" . Delimiter . "\s*)*)")
   InStr(FileExist(file), "A") ? dir:=dirname(file) : abort("Cant find config-->" file)
   if error
      return 0
   FileEncoding, UTF-8
	if (file:=FileOpen(file, "r `n")) {
		while (!file.AtEOF())
		{
         tag:=false
			try:=trim_all(line:=StrReplace(file.ReadLine(),A_Tab,"    "))
         if (SubStr(try,1,1)=":") {
            txt ? txt:=false
            tag:=true, (SubStr(try,2,1)=">") ? txt:=true
         } else if !(txt||try) {
	         continue
			} else if multi_note {
			   (SubStr(try,-1)="*#") ? multi_note:=false
			   continue
			} else if (SubStr(try,1,1)="#") {
            if (SubStr(try,2,1)="*") {
               multi_note:=true
            } else if RegExMatch(try,"^#include\s+\K\S.*$",include){
               include:=Trim(include)
               InStr(FileExist(dir "\" include), "A") ? include:=GetFullPathName(dir "\" include) : InStr(FileExist(include), "A") ? include:=GetFullPathName(include) : include:=""
               include ? (extralibs ? extralibs.="`n" : false, extralibs.="#include " include) : false
            }
			   continue
			}
         action:=""
		   if tag {
            line:=StrReplace(try, A_Space) . "`r`n"
         } else {
            ; This analysis is superficial focused on the simplification of the indentation and obtaining simple parameters.
            _pos:=1, _extra:=0, _escape:=[], _result:=[], _stringtmp:=[]
            while,(_pos:=InStr(line,Escape,,_pos+_extra)) {
               if (_end:=Substr(line,_pos+1,1)) {
                     _escape.Push(Escape _end), _max:=_escape.MaxIndex()
                     _max:="[&_" . _max . "_&]",line:=RegExReplace(line,".{2}",_max,,1,_pos), _extra:=StrLen(_max)
               } else {
                  break
               }
            }
            _pos:=1, _extra:=0
            while,(_pos:=RegExMatch(line,regex_expr,_char,_pos+_extra))
               _char1:=with_indent(with_expr(_char1)), _result.Push(solve_escape(_char1, _escape)), _max:="[``_" . _result.MaxIndex() . "_``]", line:=RegExReplace(line,".{" . StrLen(_char)-3 . "}",_max,,1,_pos+2), _extra:=StrLen(_max)+3
            _pos:=1, _extra:=0
            while,(_pos:=InStr(line,"""",,_pos+_extra))
            {
               if (_end:=InStr(line,"""",,_pos+1)) {
                  _string:=Substr(line,_pos+1,(_end-_pos)-1)
                  if (_string!="") {
                     InStr(_string,"[&_") ? _string:=solve_escape(_string, _escape)
                     _stringtmp.Push(_string), _string:="", _string_rpl:="""[&_" . _stringtmp.MaxIndex() . "_&]"""
                     line:=RegExReplace(line,".{" . (_end-_pos)+1 . "}",_string_rpl,,1,_pos), _extra:=StrLen(_string_rpl)
                  } else {
                     _extra:=2
                  }
               } else {
                  abort("Reason: A closure was expected--->""`n`n---> " try)
                  break 2
               }
            }
            _escape:=_stringtmp, _stringtmp:=""
            (_pos:=InStr(line,"#")) ? line:=Substr(line,1,_pos-1)
            _line:=StrSplit(line,";"), line:=""
            for count, eachline in _line {
                  _pos:=1,_extra:=0,main_action:={},action:="",eachline:=with_indent(eachline)
                  while (_pos:=RegExMatch(eachline,regex_main,_char,_pos+_extra))
                  {
                     _extra:=Strlen(_char)
                     if (_pos=1) {
                        action:=(_at:=InStr(_char1,Chr(1))) ? SubStr(_char1,_at+1) : _char1
                     } else {
                        main_action.Push(solve_escape(solve_escape(_char1, _escape), _result, "``"))
                     }
                     parameter:=StrSplit(_char2,Delimiter)
                     parameter[1] ? ARGS_N:=1 : ARGS_N:=0
                     parameter_end:=parameter.MaxIndex()
                     parameter[parameter_end] ? parameter_end:=false
                     for count, value in parameter {
                        (Mod(ARGS_N, 2)&&count!=parameter_end) ? main_action.Push(solve_escape(solve_escape(value, _escape), _result, "``"))
                        ARGS_N++
                     }
                     parameter:=""
                  } 
                  switch (action)
                  {
                     case "import", "importar":
                        for count, value in main_action
                           content:=read_file(InStr(FileExist(dir "\" value), "A") ? dir "\" value : value) . "`r`n" . content
                        continue
                     case "escape":
                        if (StrLen(main_action.1)=1) {
                           Escape:=main_action.1
                        } else {
                           abort("Reason: Invalid escape character--->" main_action.1 "`n`n---> " try)
                           break 2
                        }
                  }
                  main_action:=""
                  line.=solve_escape(solve_escape(eachline, _escape), _result, "``") . "`r`n"
            }
            _line:=""
         }
         content.=line
		}
		file.Close()
	}
	return content
}
build() {
   Gui 1: Submit, NoHide
   libs:="", extralibs:="", Escape:="\", error:=false
   (out="") ? out:=dirname(config) "\" simplename(config) ".exe"
   (compress="") ? compress:=2
   (SetOrigFilename="") ? SetOrigFilename := basename(config)
   admin ? extraprops:=";@Ahk2Exe-UpdateManifest 1"
   ;console ? InStr(FileExist(dinocode "\plugins\console.ahk"), "A") ? extralibs:="#include " GetFullPathName(dinocode "\plugins\console.ahk")
   Loop, %dinocode%/*.ahk
   {
      libs ? libs.="`n"
      libs.="#include " . A_LoopFileName
   }
   FileDelete, % tmp
   FileDelete, % out
   InStr(FileExist(out), "A") ? abort("The file " basename(out) " is busy")
   if error
      return 0
   FileAppend,
(
#NoEnv
#SingleInstance Ignore
SendMode Input
SetWorkingDir `%A_ScriptDir`%
SetBatchLines, -1
%libs%
;@Ahk2Exe-SetName         %SetName%
;@Ahk2Exe-SetDescription  %SetDescription%
;@Ahk2Exe-SetCopyright    %SetCopyright%
;@Ahk2Exe-SetCompanyName  %SetCompanyName%
;@Ahk2Exe-SetOrigFilename %SetOrigFilename%
;@Ahk2Exe-SetMainIcon %icon%
;@Ahk2Exe-AddResource %icon%, 160
;@Ahk2Exe-AddResource %icon%, 206
;@Ahk2Exe-AddResource %icon%, 207
;@Ahk2Exe-AddResource %icon%, 208
%extraprops%

), % tmp
   FileAppend, % "load_config(Crypt.Encrypt.StrDecrypt(""" . Crypt.Encrypt.StrEncrypt(read_file(config),"petecito",CryptAlg:=1,HashAlg:=1) . """,""petecito"",CryptAlg:=1,HashAlg:=1))`n" extralibs "`nExitApp", % tmp  
   if !error {
      RunWait, % ahk2exe . " /in """ . tmp . """ /out """ out """ /bin """ bin """" (compress ? " /compress " compress : "")
      FileDelete, % tmp
      (!InStr(FileExist(out), "A")) ? abort("Cant compile-->" config)
   }
}
with_gui(){
   global
   with_gui:=true
   Gui, +LastFound
   Gui, Color, 0xDCDCDC, 0xDDF8D2
   Gui, margin, 10, 10
   Gui, Font, s20, Arial Black
   Gui, Add, Text, Y+0 c1BD682 Section, DinoCode
   Gui, Font, s10, %style%
   Gui, Add, Text, X+0 YP+10 c1FB012 vline,----------------------------------------
   Gui, Add, Button, center X+0 YS+5 h30 w90 gbuild, BUILD
   Gui, Add, Text, Y+20 XS+10 cgreen Section, Script:
   Gui, Add, Text, Y+10 XP cgreen, Icon:
   Gui, Add, Text, Y+10 XP cgreen, Description:
   Gui, Add, Text, Y+10 XP cgreen, Copyright:
   Gui, Add, Text, Y+10 XP cgreen, Company:
   Gui, Add, Text, Y+10 XP cgreen, OrigName:
   Gui, Font, s10, Arial Black
   Gui, Add, Text, X+20 YS c49CC14, -►
   Gui, Add, Text, XP Y+10 c49CC14, -►
   Gui, Add, Text, XP Y+10 c49CC14, -►
   Gui, Add, Text, XP Y+10 c49CC14, -►
   Gui, Add, Text, XP Y+10 c49CC14, -►
   Gui, Add, Text, XP Y+10 c49CC14, -►
   Gui, Add, ListView, X+10 YS w300 h20 c747070 0x200 gselect_script -Hdr -LV0x20 ReadOnly vlist1, |
   Gui, Add, ListView, XP Y+7 w300 h20 c747070 0x200 gselect_icon -Hdr -LV0x20 ReadOnly vlist2, |
   Gui, Add, Edit, XP Y+7 w300 h20 c747070 0x200 vSetDescription center, %SetDescription%
   Gui, Add, Edit, XP Y+7 w300 h20 c747070 0x200 vSetCopyright center, %SetCopyright%
   Gui, Add, Edit, XP Y+7 w300 h20 c747070 0x200 vSetCompanyName center, %SetCompanyName%
   Gui, Add, Edit, XP Y+7 w300 h20 c747070 0x200 vSetOrigFilename center, %SetOrigFilename%
   Gui, Font, s10, %style%
   Gui, Add, CheckBox, XP Y+10 c747070 vadmin AltSubmit, Request admin permissions
   ;Gui, Add, CheckBox, XP Y+10 c747070 vconsole AltSubmit, Console mode
   Gui, show, AutoSize, DinoCC
   Gui, ListView, list2
   LV_Add("",basename(icon))
   LV_ModifyCol(1,"AutoHdr Center")
   if config {
      Gui, ListView, list1
      LV_Add("",basename(config))
      LV_ModifyCol(1,"AutoHdr Center")
   }
   WinWaitClose, DinoCC
   Gui, Destroy
}
return

select_script:
   Gui, ListView, list1
   FileSelectFile, configtmp, 1,, Please select some .config, (*.config)
   if configtmp {
      config:=configtmp, SetOrigFilename:=basename(config), out:=dirname(config) "\" simplename(config) ".exe"
      LV_GetCount() ? LV_Modify(1,"Col1",SetOrigFilename) : LV_Add("",SetOrigFilename)
      LV_ModifyCol(1,"AutoHdr Center Select")
      GuiControl, 1:, SetOrigFilename, % SetOrigFilename
   }
return

select_icon:
   Gui, ListView, list2
   FileSelectFile, icontmp, 1,, Please select some .ico, (*.ico)
   if icontmp {
      icon:=icontmp
      LV_GetCount() ? LV_Modify(1,"Col1",basename(icon)) : LV_Add("",basename(icon))
      LV_ModifyCol(1,"AutoHdr Center Select")
   }
return

GuiClose:
ExitApp