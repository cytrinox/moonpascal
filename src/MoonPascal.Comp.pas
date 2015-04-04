unit MoonPascal.Comp;

{
  Copyright (c) 2015 Daniel Vogelbacher <daniel@vogelbacher.name>
  Copyright (c) 2009-2015 Dennis D. Spreen (http://blog.spreendigital.de/)

  MoonPascal is based on the concept and interfaces of VerySimple.Lua,
  originally written by Dennis D. Spreen - but uses a different approach to
  realise dynamic generation of Delphi interfaces for Lua.
  It has improved support for registering delphi class and object methods,
  support for Lua namespaces (multiple sublevels), automatically mapping
  of enumeration types and garbage collection.


  LICENSE

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
}

interface

uses
  Dialogs,
  Winapi.Windows,
  System.Rtti,
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.TypInfo,
  Generics.Collections,
  MoonPascal.extlib;

type
  TOnLuaPrint = TGetStrProc;

  ELuaLibraryNotFound = class(Exception);
  ELuaLibraryLoadError = class(Exception);
  ELuaLibraryMethodNotFound = class(Exception);

  TMoonPascal = class(TObject)
  private
    FLuaState    : Lua_State;
    FOnPrint     : TOnLuaPrint;
    FScriptPath  : String;
    FAutoRegister: boolean;
    FActive      : boolean;
  protected
    procedure DoPrint(Msg: String); virtual;
    class procedure FreeLuaLibrary; virtual;
    class function LuaLibraryLoaded: boolean; virtual;
    class procedure RegisterPackageInternal(L: Lua_State; Data: Pointer; Code: Pointer; PackageName: String); overload; virtual;
    class function ValidMethod(Method: TRttiMethod): boolean;
    class procedure PushFunction(L: Lua_State; Data, Code: Pointer; FuncName: String);
  public
    { Basic routines }
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Open; virtual;
    procedure Close; virtual;
    class procedure LoadLuaLibrary(const ALibraryPath: String); virtual;

    { Load and/or run Lua code }
    function RunFile(Filename: String): Integer; virtual;
    function RunString(Value: String): Integer; virtual;
    function RunChunk(L: Lua_State; Status: Integer): Integer; virtual;
    function DoCall(L: Lua_State; NArg, NRes: Integer): Integer; virtual;
    function LoadFile(Filename: String): Integer; virtual;
    function Run: Integer; virtual;
    procedure ResetLuaState; virtual;

    procedure PushUserData(APointer: TObject; const AMetaID: string); overload; virtual;
    class procedure PushUserData(L: Lua_State; APointer: TObject; const AMetaID: string); overload; virtual;

    procedure PushLightUserData(APointer: TObject; const AMetaID: string); overload; virtual;
    class procedure PushLightUserData(L: Lua_State; APointer: TObject; const AMetaID: string); overload; virtual;

    { Register various types of functions, methods and types }
    class procedure RegisterMethod(L: Lua_State; Data: Pointer; Code: Pointer; FuncName: String; ANamespace: array of String); overload; virtual;
    class procedure RegisterMethod(L: Lua_State; AObject: TObject; FuncName: String; ANamespace: array of String); overload; virtual;
    class procedure RegisterStaticFunction(L: Lua_State; Code: Pointer; FuncName: String; ANamespace: array of String); overload; virtual;
    class procedure RegisterClassFunction(L: Lua_State; AClass: TClass; FuncName: string; ANamespace: array of String); overload; virtual;
    class procedure RegisterClassFunction(L: Lua_State; AObject: TObject; FuncName: String; ANamespace: array of String); overload; virtual;
    class procedure RegisterType(L: Lua_State; AClass: TClass; ATypeName: string); overload; virtual;
    class procedure RegisterEnum(L: Lua_State; ATypeInfo: PTypeInfo; ANamespace: array of string; AModifier: TFunc<String, String>); overload; virtual;

    procedure RegisterMethod(AObject: TObject; FuncName: String; ANamespace: array of String); overload; virtual;
    procedure RegisterStaticFunction(AEntryPoint: Pointer; FuncName: String; ANamespace: array of String); overload; virtual;
    procedure RegisterClassFunction(AClass: TClass; FuncName: string; ANamespace: array of String); overload; virtual;
    procedure RegisterClassFunction(AObject: TObject; FuncName: String; ANamespace: array of String); overload; virtual;
    procedure RegisterType(AClass: TClass; ATypeName: string = ''); overload; virtual;
    procedure RegisterEnum(ATypeInfo: PTypeInfo; ANamespace: array of string; AModifier: TFunc<String, String> = nil); overload; virtual;

    { Register all published methods and functions from class or object instance }
    class procedure RegisterAll(L: Lua_State; AClass: TClass; ANamespace: array of String); overload; virtual;
    class procedure RegisterAll(L: Lua_State; AObject: TObject; ANamespace: array of String); overload; virtual;

    procedure RegisterAll(AClass: TClass; ANamespace: array of String); overload; virtual;
    procedure RegisterAll(AObject: TObject; ANamespace: array of String); overload; virtual;

    { Register class or object instance as Lua package }
    // Register all published functions for a Package table on the stack
    class procedure RegisterPackageFunctions(L: Lua_State; AObject: TObject); overload; virtual;
    // Register a Package with a specific package loader (cdecl static function)
    class procedure RegisterPackage(L: Lua_State; PackageName: String; InitFunc: lua_CFunction); overload; virtual;
    // Register a Package with a specific object package loader
    class procedure RegisterPackage(L: Lua_State; PackageName: String; AObject: TObject; PackageLoader: String); overload; virtual;
    // Register a Package with the default package loader (auto register all published functions)
    class procedure RegisterPackage(L: Lua_State; PackageName: String; AObject: TObject); overload; virtual;

    procedure RegisterPackage(PackageName: String; InitFunc: lua_CFunction); overload; inline;
    procedure RegisterPackage(PackageName: String; AObject: TObject; PackageLoader: String); overload; inline;
    procedure RegisterPackage(PackageName: String; AObject: TObject); overload; inline;

    { Properties }
    property LuaState: Lua_State read FLuaState write FLuaState;
    property OnPrint: TOnLuaPrint read FOnPrint write FOnPrint;
    property ScriptPath: String read FScriptPath write FScriptPath;
    property AutoRegister: boolean read FAutoRegister write FAutoRegister;
    property Active: boolean read FActive;

  published
    { Builtin functions }
    function Print(L: Lua_State): Integer;
  end;

implementation

type
  TLuaProc = function(L: Lua_State): Integer of object;

  TManagedInstance = record
    FObject: TObject;
  end;

  PManagedInstance = ^TManagedInstance;

var
  { Unit global var for the Link Library handle }
  LibraryHandle: HMODULE;

  { **** LOCAL FUNCTIONS AND HELPERS **** }

  // This function is called by Lua, it extracts the object by
  // pointer to the objects method by name, which is then called.
function LuaCallBack(L: Lua_State): Integer; cdecl;
var
  LRoutine: TMethod; // Code and Data for the call back method
begin
  // Retrieve closure values (=object Pointer)
  LRoutine.Data := lua_topointer(L, lua_upvalueindex(1));
  LRoutine.Code := lua_topointer(L, lua_upvalueindex(2));
  // Call object function
  Result := TLuaProc(LRoutine)(L);
end;

// Creates a new Lua package
function LuaLoadPackage(L: Lua_State): Integer; cdecl;
var
  Obj: TObject;
begin
  // Retrieve closure values (=object Pointer)
  Obj := lua_topointer(L, lua_upvalueindex(1));

  lua_newtable(L);
  TMoonPascal.RegisterPackageFunctions(L, Obj);

  Result := 1; // Return table
end;

// Message handler used to run all chunks
function MsgHandler(L: Lua_State): Integer; cdecl;
var
  Msg: MarshaledAString;
begin
  Msg := lua_tostring(L, 1);
  if (Msg = NIL) then                               // * is error object not a string?
    if (luaL_callmeta(L, 1, '__tostring') <> 0) and // * does it have a metamethod */
      (lua_type(L, -1) = LUA_TSTRING) then          // * that produces a string? */
    begin
      Result := 1; // * that is the message */}
      Exit;
    end
    else
      Msg := lua_pushfstring(L, '(error object is a %s value)', [luaL_typename(L, 1)]);

  luaL_traceback(L, L, Msg, 1); // * append a standard traceback */
  Result := 1;                  // * return the traceback */
end;

// Get function pointer from module by name
function FindEntryPoint(Name: String): Pointer;
begin
  Result := GetProcAddress(LibraryHandle, PWideChar(Name));
  if not Assigned(Result) then
    raise ELuaLibraryMethodNotFound.CreateFmt('Entry point "%s" not found', [Name]);
end;

// Calls a method on instance by name instead of a pointer
function TypeMethodDispatcher(L: Lua_State): Integer; cdecl;
var
  Routine   : TMethod; // Code and Data for the call back method
  MethodName: String;
  ClassName : String;
  ObjPtr    : TObject;
  Marshall  : TMarshaller;
begin
  MethodName := String(lua_tostring(L, lua_upvalueindex(1))); // method name
  ClassName  := String(lua_tostring(L, lua_upvalueindex(2))); // class name
  ObjPtr     := luaL_checkudata(L, -1, Marshall.AsAnsi(ClassName).ToPointer);

  if lua_islightuserdata(L, -1) then
  begin
    Routine.Data := ObjPtr;
    Routine.Code := ObjPtr.MethodAddress(MethodName);
    Assert(Routine.Code <> nil);
  end
  else
  begin
    Routine.Data := PManagedInstance(ObjPtr)^.FObject;
    Routine.Code := PManagedInstance(ObjPtr)^.FObject.MethodAddress(MethodName);
    Assert(Routine.Code <> nil);
  end;

  // Call object function
  Result := TLuaProc(Routine)(L);
end;

function gc(L: Lua_State): Integer; cdecl;
var
  ObjPtr: TObject;
begin
  if not lua_islightuserdata(L, -1) then
  begin
    ObjPtr := lua_touserdata(L, -1);
    PManagedInstance(ObjPtr)^.FObject.Free;
  end;
end;

{ **** CLASS METHODS **** }

constructor TMoonPascal.Create;
begin
  FAutoRegister := True;
end;

destructor TMoonPascal.Destroy;
begin
  self.Close;
  inherited;
end;

procedure TMoonPascal.Open;
begin
  if FActive then
    Exit;

  if not LuaLibraryLoaded then
    LoadLuaLibrary(LUA_LIBRARY);

  // Open Lua library
  LuaState := lual_newstate();
  lual_openlibs(LuaState);

  FActive := True;

  // Override print method
  RegisterMethod(LuaState, self, self.MethodAddress('print'), 'print', []);
end;

procedure TMoonPascal.Close;
begin
  if not FActive then
    Exit;
  Lua_Close(LuaState);
  FActive := False;
end;

function TMoonPascal.Print(L: Lua_State): Integer;
var
  N, I: Integer;
  S   : MarshaledAString;
  Sz  : size_t;
  Msg : String;
begin
  Msg := '';

  N := lua_gettop(L); // * number of arguments */
  lua_getglobal(L, 'tostring');
  for I := 1 to N do
  begin
    lua_pushvalue(L, -1); // * function to be called */
    lua_pushvalue(L, I);  // * value to print */
    lua_call(L, 1, 1);
    S := lua_tolstring(L, -1, @Sz); // * get result */
    if S = NIL then
    begin
      Result := luaL_error(L, '"tostring" must return a string to "print"', []);
      Exit;
    end;

    if I > 1 then
      Msg := Msg + #9;
    Msg   := Msg + String(S);
    lua_pop(L, 1); // * pop result */
  end;
  Result := 0;

  DoPrint(Msg);
end;

function TMoonPascal.RunChunk(L: Lua_State; Status: Integer): Integer;
begin
  if Status = LUA_OK then
    Status := DoCall(L, 0, 0);
  Result   := Status;
end;

// Interface to 'lua_pcall', which sets appropriate message function
// and C-signal handler. Used to run all chunks.
function TMoonPascal.DoCall(L: Lua_State; NArg, NRes: Integer): Integer;
var
  Status: Integer;
  Base  : Integer;
begin
  Base := lua_gettop(L) - NArg;     // function index
  lua_pushcfunction(L, MsgHandler); // push message handler
  lua_insert(L, Base);              // put it under function and args
  Status := lua_pcall(L, NArg, NRes, Base);
  lua_remove(L, Base); // remove message handler from the stack */
  Result := Status;
end;

function TMoonPascal.RunFile(Filename: String): Integer;
var
  Marshall: TMarshaller;
  Path    : String;
begin
  if not Active then
    Open;

  Path   := Filename;
  Result := RunChunk(LuaState, lual_loadfile(LuaState, Marshall.AsAnsi(Path).ToPointer));
end;

procedure TMoonPascal.DoPrint(Msg: String);
begin
  if Assigned(FOnPrint) then
    FOnPrint(Msg)
  else
    ShowMessage(Msg);
end;

function TMoonPascal.RunString(Value: String): Integer;
var
  Marshall: TMarshaller;
begin
  if not Active then
    Open;

  Result := luaL_dostring(LuaState, Marshall.AsAnsi(Value).ToPointer);
end;

class procedure TMoonPascal.PushFunction(L: Lua_State; Data, Code: Pointer; FuncName: String);
var
  Marshall: TMarshaller;
begin
  // prepare Closure value (Method Name)
  lua_pushstring(L, Marshall.AsAnsi(FuncName).ToPointer);

  // prepare Closure value (CallBack Object Pointer)
  lua_pushlightuserdata(L, Data);
  lua_pushlightuserdata(L, Code);

  // set new Lua function with Closure values
  lua_pushcclosure(L, LuaCallBack, 2);
end;

procedure TMoonPascal.PushLightUserData(APointer: TObject; const AMetaID: string);
begin
  PushLightUserData(self.LuaState, APointer, AMetaID);
end;

class procedure TMoonPascal.PushLightUserData(L: Lua_State; APointer: TObject; const AMetaID: string);
var
  Marshall: TMarshaller;
begin
  lua_pushlightuserdata(L, Pointer(APointer));
  luaL_setmetatable(L, Marshall.AsAnsi(AMetaID).ToPointer);
end;

procedure TMoonPascal.PushUserData(APointer: TObject; const AMetaID: string);
begin
  PushUserData(self.LuaState, APointer, AMetaID);
end;

class procedure TMoonPascal.PushUserData(L: Lua_State; APointer: TObject; const AMetaID: string);
var
  Marshall    : TMarshaller;
  LManagedData: PManagedInstance;
begin
  LManagedData          := lua_newuserdata(L, sizeof(PManagedInstance));
  LManagedData^.FObject := APointer;
  // lua_pushlightuserdata(L, Pointer(APointer));
  luaL_setmetatable(L, Marshall.AsAnsi(AMetaID).ToPointer);
end;

class function TMoonPascal.ValidMethod(Method: TRttiMethod): boolean;
var
  Params: TArray<TRttiParameter>;
  Param : TRttiParameter;
begin
  Result := False;

  // Only published functions with an Integer result allowed
  if (Method.Visibility <> mvPublished) or (not Assigned(Method.ReturnType)) or (Method.ReturnType.TypeKind <> tkInteger) then
    Exit;

  // Only functions with 1 parameter allowed
  Params := Method.GetParameters;
  if Length(Params) <> 1 then
    Exit;

  // Only functions with a Pointer as parameter allowed
  Param := Params[0];
  if Param.ParamType.TypeKind <> tkPointer then
    Exit;

  Result := True;
end;

procedure TMoonPascal.RegisterType(AClass: TClass; ATypeName: string);
begin
  RegisterType(self.LuaState, AClass, ATypeName);
end;

procedure TMoonPascal.ResetLuaState;
begin
  if self.Active then
  begin
    self.Close;
    self.Open;
  end;
end;

class procedure TMoonPascal.RegisterType(L: Lua_State; AClass: TClass; ATypeName: string);
var
  LContext: TRttiContext;
  LType   : TRttiType;
  LMethod : TRttiMethod;
  Marshall: TMarshaller;
begin
  if ATypeName = '' then
    ATypeName := AClass.ClassName;
  LContext    := TRttiContext.Create;
  try
    luaL_newmetatable(L, Marshall.AsAnsi(ATypeName).ToPointer);
    lua_pushvalue(L, -1);
    lua_setfield(L, -2, '__index');

    lua_pushcfunction(L, gc);
    lua_setfield(L, -2, '__gc');

    LType := LContext.GetType(AClass);
    for LMethod in LType.GetMethods do
    begin
      // Only published functions with an Integer result allowed
      if not ValidMethod(LMethod) then
        Continue;

      // Register the method based on calling convention and method kind
      if LMethod.MethodKind = mkFunction then
      begin
        lua_pushstring(L, Marshall.AsAnsi(LMethod.Name).ToPointer); // method name
        lua_pushvalue(L, -1); // dup for closure
        lua_pushstring(L, Marshall.AsAnsi(ATypeName).ToPointer); // class name for closure
        lua_pushcclosure(L, TypeMethodDispatcher, 2);
        lua_settable(L, -3); // closure, method-name into table at -3
      end;
    end;
    lua_pop(L, -1);
  finally
    LContext.Free;
  end;
end;

class procedure TMoonPascal.RegisterStaticFunction(L: Lua_State; Code: Pointer; FuncName: String; ANamespace: array of String);
var
  Marshall  : TMarshaller;
  I         : Integer;
  E         : Integer;
  LStackSize: Integer;
begin
  LStackSize := lua_gettop(L);
  lua_pushglobaltable(L); // starting from global
  for I := Low(ANamespace) to High(ANamespace) do
  begin
    lua_pushstring(L, Marshall.AsAnsi(ANamespace[I]).ToPointer);
    E := lua_gettable(L, -1);
    case E of
      LUA_TTABLE:
        // nothing to do, next table is on top of stack
        Break;
      LUA_TNIL:
        begin
          // remove nil table first
          lua_pop(L, 1);
          // create new table
          lua_newtable(L);
          lua_pushstring(L, Marshall.AsAnsi(ANamespace[I]).ToPointer);
          lua_pushvalue(L, -2); // dup table ref
          lua_settable(L, -4);  // set in previous table
          // now new table is on top
          Break;
        end
    else
      raise Exception.CreateFmt('The table for namespace %s is of invalid type', [ANamespace[I]]);
    end;
  end;
  luaL_checktype(L, -1, LUA_TTABLE);

  lua_pushstring(L, Marshall.AsAnsi(FuncName).ToPointer);
  // set new Lua function with Closure values
  lua_pushcclosure(L, Code, 0);

  lua_settable(L, -3);
  lua_settop(L, LStackSize);
end;

class procedure TMoonPascal.RegisterMethod(L: Lua_State; Data, Code: Pointer; FuncName: String; ANamespace: array of String);
var
  Marshall  : TMarshaller;
  I         : Integer;
  E         : Integer;
  LStackSize: Integer;
begin
  LStackSize := lua_gettop(L);
  lua_pushglobaltable(L); // starting from global
  for I := Low(ANamespace) to High(ANamespace) do
  begin
    lua_pushstring(L, Marshall.AsAnsi(ANamespace[I]).ToPointer);
    E := lua_gettable(L, -1);
    case E of
      LUA_TTABLE:
        ;
      // nothing to do, next table is on top of stack
      LUA_TNIL:
        begin
          // remove nil table first
          lua_pop(L, 1);
          // create new table
          lua_newtable(L);
          lua_pushstring(L, Marshall.AsAnsi(ANamespace[I]).ToPointer);
          lua_pushvalue(L, -2); // dup table ref
          lua_settable(L, -4);  // set in previous table
          // now new table is on top
        end
    else
      raise Exception.CreateFmt('The table for namespace %s is of invalid type', [ANamespace[I]]);
    end;
  end;
  luaL_checktype(L, -1, LUA_TTABLE);
  PushFunction(L, Data, Code, FuncName);
  lua_settable(L, -3);
  lua_settop(L, LStackSize);
end;

function TMoonPascal.Run: Integer;
var
  Msg: PAnsiChar;
begin
  Result := RunChunk(LuaState, LUA_OK);
  if Result <> 0 then
  begin
    Msg := lua_tostring(LuaState, -1);
    ShowMessage(string(Msg));
  end;
end;

class procedure TMoonPascal.RegisterEnum(L: Lua_State; ATypeInfo: PTypeInfo; ANamespace: array of string; AModifier: TFunc<String, String>);
var
  I         : Integer;
  LStr      : string;
  Marshall  : TMarshaller;
  J         : Integer;
  E         : Integer;
  LStackSize: Integer;
begin
  LStackSize := lua_gettop(L);
  lua_pushglobaltable(L); // starting from global
  for I := Low(ANamespace) to High(ANamespace) do
  begin
    lua_pushstring(L, Marshall.AsAnsi(ANamespace[I]).ToPointer);
    E := lua_gettable(L, -1);
    case E of
      LUA_TTABLE:
        ;
      // nothing to do, next table is on top of stack
      LUA_TNIL:
        begin
          // remove nil table first
          lua_pop(L, 1);
          // create new table
          lua_newtable(L);
          lua_pushstring(L, Marshall.AsAnsi(ANamespace[I]).ToPointer);
          lua_pushvalue(L, -2); // dup table ref
          lua_settable(L, -4);  // set in previous table
          // now new table is on top
        end
    else
      raise Exception.CreateFmt('The table for namespace %s is of invalid type', [ANamespace[I]]);
    end;
  end;
  luaL_checktype(L, -1, LUA_TTABLE);

  // iterate over all enum vars
  with GetTypeData(ATypeInfo)^ do
    for J := MinValue to MaxValue do
    begin
      LStr := GetEnumName(ATypeInfo, Ord(J));
      if LStr <> '' then
      begin
        // Modify name, e.g. the user can add a prefix oder translate to upper case
        if Assigned(AModifier) then
          LStr := AModifier(LStr);
        lua_pushstring(L, Marshall.AsAnsi(LStr).ToPointer);
        lua_pushinteger(L, Ord(J));
        lua_settable(L, -3);
      end;
    end;

  lua_settop(L, LStackSize);
end;

class procedure TMoonPascal.RegisterAll(L: Lua_State; AClass: TClass; ANamespace: array of String);
var
  LContext: TRttiContext;
  LType   : TRttiType;
  LMethod : TRttiMethod;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AClass);
    for LMethod in LType.GetMethods do
      if ValidMethod(LMethod) then
        RegisterClassFunction(L, AClass, LMethod.Name, ANamespace);
  finally
    LContext.Free;
  end;
end;

class procedure TMoonPascal.RegisterAll(L: Lua_State; AObject: TObject; ANamespace: array of String);
var
  LContext: TRttiContext;
  LType   : TRttiType;
  LMethod : TRttiMethod;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AObject.ClassType);
    for LMethod in LType.GetMethods do
    begin
      // Only published functions with an Integer result allowed
      if not ValidMethod(LMethod) then
        Continue;

      // Register the method based on calling convention and method kind
      if LMethod.MethodKind = mkFunction then
        RegisterMethod(L, AObject, LMethod.Name, ANamespace)
      else
        if LMethod.MethodKind = mkClassFunction then
          if (LMethod.IsStatic) and (LMethod.CallingConvention = ccCdecl) then
            RegisterStaticFunction(L, AObject.MethodAddress(LMethod.Name), LMethod.Name, ANamespace)
          else
            RegisterClassFunction(L, AObject.ClassType, LMethod.Name, ANamespace);
    end;
  finally
    LContext.Free;
  end;
end;

class procedure TMoonPascal.RegisterPackageFunctions(L: Lua_State; AObject: TObject);
var
  LContext: TRttiContext;
  LType   : TRttiType;
  LMethod : TRttiMethod;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AObject.ClassType);
    for LMethod in LType.GetMethods do
    begin
      // Only published functions with an Integer result allowed
      if not ValidMethod(LMethod) then
        Continue;

      PushFunction(L, AObject, LMethod.CodeAddress, LMethod.Name);
      lua_rawset(L, -3);
    end;
  finally
    LContext.Free;
  end;
end;

class procedure TMoonPascal.RegisterPackage(L: Lua_State; PackageName: String; AObject: TObject);
var
  Marshall: TMarshaller;
begin
  lua_getglobal(L, 'package');    // get local package table
  lua_getfield(L, -1, 'preload'); // get preload field

  // prepare Closure value (Object Object Pointer)
  lua_pushlightuserdata(L, AObject);

  // set new Lua function with Closure values
  lua_pushcclosure(L, LuaLoadPackage, 1);

  lua_setfield(L, -2, Marshall.AsAnsi(PackageName).ToPointer);
  lua_pop(L, 2);
end;

class procedure TMoonPascal.RegisterPackage(L: Lua_State; PackageName: String; InitFunc: lua_CFunction);
var
  Marshall: TMarshaller;
begin
  lua_getglobal(L, 'package');    // get local package table
  lua_getfield(L, -1, 'preload'); // get preload field
  lua_pushcfunction(L, InitFunc);
  lua_setfield(L, -2, Marshall.AsAnsi(PackageName).ToPointer);
  lua_pop(L, 2);
end;

class procedure TMoonPascal.RegisterPackageInternal(L: Lua_State; Data, Code: Pointer; PackageName: String);
var
  Marshall: TMarshaller;
begin
  lua_getglobal(L, 'package');    // get local package table
  lua_getfield(L, -1, 'preload'); // get preload field

  // prepare Closure value (CallBack Object Pointer)
  lua_pushlightuserdata(L, Data);
  lua_pushlightuserdata(L, Code);

  // set new Lua function with Closure values
  lua_pushcclosure(L, LuaCallBack, 2);

  lua_setfield(L, -2, Marshall.AsAnsi(PackageName).ToPointer);
  lua_pop(L, 2);
end;

class procedure TMoonPascal.RegisterPackage(L: Lua_State; PackageName: String; AObject: TObject; PackageLoader: String);
var
  LContext: TRttiContext;
  LType   : TRttiType;
  LMethod : TRttiMethod;
  Address : Pointer;
begin
  LContext := TRttiContext.Create;
  try
    LType   := LContext.GetType(AObject.ClassType);
    LMethod := LType.GetMethod(PackageLoader);
    Address := LMethod.CodeAddress;
    RegisterPackageInternal(L, AObject, Address, PackageName);
  finally
    LContext.Free;
  end;
end;

function TMoonPascal.LoadFile(Filename: String): Integer;
var
  Marshall: TMarshaller;
  Msg     : PAnsiChar;
begin
  Result := lual_loadfile(LuaState, Marshall.AsAnsi(Filename).ToPointer);
  if Result <> 0 then
  begin
    Msg := lua_tostring(LuaState, -1);
    ShowMessage(string(Msg));
  end;
end;

class procedure TMoonPascal.LoadLuaLibrary(const ALibraryPath: String);
var
  LLoadPath: String;
begin
  FreeLuaLibrary;

  if ALibraryPath = '' then
    LLoadPath := LUA_LIBRARY
  else
    LLoadPath := ALibraryPath;

{$IFNDEF STATICLIBRARY}
  // check if Library exists
  if not FileExists(LLoadPath) then
    raise ELuaLibraryNotFound.CreateFmt('Lua library "%s" not found', [LLoadPath]);

  // try to load the library
  LibraryHandle := LoadLibrary(PChar(LLoadPath));
  if LibraryHandle = 0 then
    raise ELuaLibraryLoadError.CreateFmt('Failed to load Lua library "%s"', [LLoadPath]);

  lua_newstate  := FindEntryPoint('lua_newstate');
  Lua_Close     := FindEntryPoint('lua_close');
  lua_newthread := FindEntryPoint('lua_newthread');
  lua_atpanic   := FindEntryPoint('lua_atpanic');
  lua_version   := FindEntryPoint('lua_version');

  lua_absindex   := FindEntryPoint('lua_absindex');
  lua_gettop     := FindEntryPoint('lua_gettop');
  lua_settop     := FindEntryPoint('lua_settop');
  lua_pushvalue  := FindEntryPoint('lua_pushvalue');
  lua_rotate     := FindEntryPoint('lua_rotate');
  lua_copy       := FindEntryPoint('lua_copy');
  lua_checkstack := FindEntryPoint('lua_checkstack');
  lua_xmove      := FindEntryPoint('lua_xmove');

  lua_isnumber    := FindEntryPoint('lua_isnumber');
  lua_isstring    := FindEntryPoint('lua_isstring');
  lua_iscfunction := FindEntryPoint('lua_iscfunction');
  lua_isinteger   := FindEntryPoint('lua_isinteger');
  lua_isuserdata  := FindEntryPoint('lua_isuserdata');
  lua_type        := FindEntryPoint('lua_type');
  lua_typename    := FindEntryPoint('lua_typename');

  lua_tonumberx   := FindEntryPoint('lua_tonumberx');
  lua_tointegerx  := FindEntryPoint('lua_tointegerx');
  lua_toboolean   := FindEntryPoint('lua_toboolean');
  lua_tolstring   := FindEntryPoint('lua_tolstring');
  lua_rawlen      := FindEntryPoint('lua_rawlen');
  lua_tocfunction := FindEntryPoint('lua_tocfunction');
  lua_touserdata  := FindEntryPoint('lua_touserdata');
  lua_tothread    := FindEntryPoint('lua_tothread');
  lua_topointer   := FindEntryPoint('lua_topointer');

  lua_arith    := FindEntryPoint('lua_arith');
  lua_rawequal := FindEntryPoint('lua_rawequal');
  lua_compare  := FindEntryPoint('lua_compare');

  lua_pushnil           := FindEntryPoint('lua_pushnil');
  lua_pushnumber        := FindEntryPoint('lua_pushnumber');
  lua_pushinteger       := FindEntryPoint('lua_pushinteger');
  lua_pushlstring       := FindEntryPoint('lua_pushlstring');
  lua_pushstring        := FindEntryPoint('lua_pushstring');
  lua_pushvfstring      := FindEntryPoint('lua_pushvfstring');
  lua_pushfstring       := FindEntryPoint('lua_pushfstring');
  lua_pushcclosure      := FindEntryPoint('lua_pushcclosure');
  lua_pushboolean       := FindEntryPoint('lua_pushboolean');
  lua_pushlightuserdata := FindEntryPoint('lua_pushlightuserdata');
  lua_pushthread        := FindEntryPoint('lua_pushthread');

  lua_getglobal := FindEntryPoint('lua_getglobal');
  lua_gettable  := FindEntryPoint('lua_gettable');
  lua_getfield  := FindEntryPoint('lua_getfield');
  lua_geti      := FindEntryPoint('lua_geti');
  lua_rawget    := FindEntryPoint('lua_rawget');
  lua_rawgeti   := FindEntryPoint('lua_rawgeti');
  lua_rawgetp   := FindEntryPoint('lua_rawgetp');

  lua_createtable  := FindEntryPoint('lua_createtable');
  lua_newuserdata  := FindEntryPoint('lua_newuserdata');
  lua_getmetatable := FindEntryPoint('lua_getmetatable');
  lua_getuservalue := FindEntryPoint('lua_getuservalue');

  lua_setglobal    := FindEntryPoint('lua_setglobal');
  lua_settable     := FindEntryPoint('lua_settable');
  lua_setfield     := FindEntryPoint('lua_setfield');
  lua_seti         := FindEntryPoint('lua_seti');
  lua_rawset       := FindEntryPoint('lua_rawset');
  lua_rawseti      := FindEntryPoint('lua_rawseti');
  lua_rawsetp      := FindEntryPoint('lua_rawsetp');
  lua_setmetatable := FindEntryPoint('lua_setmetatable');
  lua_setuservalue := FindEntryPoint('lua_setuservalue');

  lua_callk  := FindEntryPoint('lua_callk');
  lua_pcallk := FindEntryPoint('lua_pcallk');
  lua_load   := FindEntryPoint('lua_load');
  lua_dump   := FindEntryPoint('lua_dump');

  lua_yieldk      := FindEntryPoint('lua_yieldk');
  lua_resume      := FindEntryPoint('lua_resume');
  lua_status      := FindEntryPoint('lua_status');
  lua_isyieldable := FindEntryPoint('lua_isyieldable');

  lua_gc := FindEntryPoint('lua_gc');

  lua_error  := FindEntryPoint('lua_error');
  lua_next   := FindEntryPoint('lua_next');
  lua_concat := FindEntryPoint('lua_concat');
  lua_len    := FindEntryPoint('lua_len');

  lua_stringtonumber := FindEntryPoint('lua_stringtonumber');
  lua_getallocf      := FindEntryPoint('lua_getallocf');
  lua_setallocf      := FindEntryPoint('lua_setallocf');

  lua_getstack    := FindEntryPoint('lua_getstack');
  lua_getinfo     := FindEntryPoint('lua_getinfo');
  lua_getlocal    := FindEntryPoint('lua_getlocal');
  lua_setlocal    := FindEntryPoint('lua_setlocal');
  lua_getupvalue  := FindEntryPoint('lua_getupvalue');
  lua_setupvalue  := FindEntryPoint('lua_setupvalue');
  lua_upvalueid   := FindEntryPoint('lua_upvalueid');
  lua_upvaluejoin := FindEntryPoint('lua_upvaluejoin');

  lua_sethook      := FindEntryPoint('lua_sethook');
  lua_gethook      := FindEntryPoint('lua_gethook');
  lua_gethookmask  := FindEntryPoint('lua_gethookmask');
  lua_gethookcount := FindEntryPoint('lua_gethookcount');

  luaopen_base      := FindEntryPoint('luaopen_base');
  luaopen_coroutine := FindEntryPoint('luaopen_coroutine');
  luaopen_table     := FindEntryPoint('luaopen_table');
  luaopen_io        := FindEntryPoint('luaopen_io');
  luaopen_os        := FindEntryPoint('luaopen_os');
  luaopen_string    := FindEntryPoint('luaopen_string');
  luaopen_utf8      := FindEntryPoint('luaopen_utf8');
  luaopen_bit32     := FindEntryPoint('luaopen_bit32');
  luaopen_math      := FindEntryPoint('luaopen_math');
  luaopen_debug     := FindEntryPoint('luaopen_debug');
  luaopen_package   := FindEntryPoint('luaopen_package');

  lual_openlibs := FindEntryPoint('luaL_openlibs');

  luaL_checkversion_ := FindEntryPoint('luaL_checkversion_');
  luaL_getmetafield  := FindEntryPoint('luaL_getmetafield');
  luaL_callmeta      := FindEntryPoint('luaL_callmeta');
  luaL_tolstring     := FindEntryPoint('luaL_tolstring');
  luaL_argerror      := FindEntryPoint('luaL_argerror');
  luaL_checklstring  := FindEntryPoint('luaL_checklstring');
  luaL_optlstring    := FindEntryPoint('luaL_optlstring');
  luaL_checknumber   := FindEntryPoint('luaL_checknumber');
  luaL_optnumber     := FindEntryPoint('luaL_optnumber');
  luaL_checkinteger  := FindEntryPoint('luaL_checkinteger');
  luaL_optinteger    := FindEntryPoint('luaL_optinteger');

  luaL_checkstack := FindEntryPoint('luaL_checkstack');
  luaL_checktype  := FindEntryPoint('luaL_checktype');
  luaL_checkany   := FindEntryPoint('luaL_checkany');

  luaL_newmetatable := FindEntryPoint('luaL_newmetatable');
  luaL_setmetatable := FindEntryPoint('luaL_setmetatable');
  luaL_testudata    := FindEntryPoint('luaL_testudata');
  luaL_checkudata   := FindEntryPoint('luaL_checkudata');

  luaL_where := FindEntryPoint('luaL_where');
  luaL_error := FindEntryPoint('luaL_error');

  luaL_checkoption := FindEntryPoint('luaL_checkoption');
  luaL_fileresult  := FindEntryPoint('luaL_fileresult');
  luaL_execresult  := FindEntryPoint('luaL_execresult');

  luaL_ref   := FindEntryPoint('luaL_ref');
  luaL_unref := FindEntryPoint('luaL_unref');

  luaL_loadfilex   := FindEntryPoint('luaL_loadfilex');
  luaL_loadbufferx := FindEntryPoint('luaL_loadbufferx');
  luaL_loadstring  := FindEntryPoint('luaL_loadstring');
  lual_newstate    := FindEntryPoint('luaL_newstate');
  luaL_len         := FindEntryPoint('luaL_len');

  luaL_gsub     := FindEntryPoint('luaL_gsub');
  luaL_setfuncs := FindEntryPoint('luaL_setfuncs');

  luaL_getsubtable := FindEntryPoint('luaL_getsubtable');
  luaL_traceback   := FindEntryPoint('luaL_traceback');
  luaL_requiref    := FindEntryPoint('luaL_requiref');

  luaL_buffinit       := FindEntryPoint('luaL_buffinit');
  luaL_prepbuffsize   := FindEntryPoint('luaL_prepbuffsize');
  luaL_addlstring     := FindEntryPoint('luaL_addlstring');
  luaL_addstring      := FindEntryPoint('luaL_addstring');
  luaL_addvalue       := FindEntryPoint('luaL_addvalue');
  luaL_pushresult     := FindEntryPoint('luaL_pushresult');
  luaL_pushresultsize := FindEntryPoint('luaL_pushresultsize');
  luaL_buffinitsize   := FindEntryPoint('luaL_buffinitsize');
{$ENDIF}
end;

class function TMoonPascal.LuaLibraryLoaded: boolean;
begin
  Result := (LibraryHandle <> 0);
end;

class procedure TMoonPascal.FreeLuaLibrary;
begin
  if LibraryHandle <> 0 then
  begin
    FreeLibrary(LibraryHandle);
    LibraryHandle := 0;
  end;
end;

class procedure TMoonPascal.RegisterMethod(L: Lua_State; AObject: TObject; FuncName: String; ANamespace: array of String);
begin
  RegisterMethod(L, AObject, AObject.MethodAddress(FuncName), FuncName, ANamespace);
end;

procedure TMoonPascal.RegisterClassFunction(AObject: TObject; FuncName: String; ANamespace: array of String);
begin
  RegisterClassFunction(self.LuaState, AObject, FuncName, ANamespace);
end;

procedure TMoonPascal.RegisterEnum(ATypeInfo: PTypeInfo; ANamespace: array of string; AModifier: TFunc<String, String>);
begin
  RegisterEnum(self.LuaState, ATypeInfo, ANamespace, AModifier);
end;

procedure TMoonPascal.RegisterClassFunction(AClass: TClass; FuncName: string; ANamespace: array of String);
begin
  RegisterClassFunction(self.LuaState, AClass, FuncName, ANamespace);
end;

procedure TMoonPascal.RegisterStaticFunction(AEntryPoint: Pointer; FuncName: String; ANamespace: array of String);
begin
  RegisterStaticFunction(self.LuaState, AEntryPoint, FuncName, ANamespace);
end;

class procedure TMoonPascal.RegisterClassFunction(L: Lua_State; AObject: TObject; FuncName: String; ANamespace: array of String);
begin
  RegisterMethod(L, AObject.ClassType, AObject.ClassType.MethodAddress(FuncName), FuncName, ANamespace);
end;

class procedure TMoonPascal.RegisterClassFunction(L: Lua_State; AClass: TClass; FuncName: string; ANamespace: array of String);
begin
  RegisterMethod(L, AClass, AClass.MethodAddress(FuncName), FuncName, ANamespace);
end;

procedure TMoonPascal.RegisterMethod(AObject: TObject; FuncName: String; ANamespace: array of String);
begin
  RegisterMethod(self.LuaState, AObject, AObject.MethodAddress(FuncName), FuncName, ANamespace);
end;

procedure TMoonPascal.RegisterAll(AClass: TClass; ANamespace: array of String);
begin
  RegisterAll(LuaState, AClass, ANamespace);
end;

procedure TMoonPascal.RegisterAll(AObject: TObject; ANamespace: array of String);
begin
  RegisterAll(LuaState, AObject, ANamespace);
end;

procedure TMoonPascal.RegisterPackage(PackageName: String; AObject: TObject);
begin
  RegisterPackage(LuaState, PackageName, AObject);
end;

procedure TMoonPascal.RegisterPackage(PackageName: String; InitFunc: lua_CFunction);
begin
  RegisterPackage(LuaState, PackageName, InitFunc);
end;

procedure TMoonPascal.RegisterPackage(PackageName: String; AObject: TObject; PackageLoader: String);
begin
  RegisterPackage(LuaState, PackageName, AObject, PackageLoader);
end;

initialization

LibraryHandle := 0;

finalization

if LibraryHandle <> 0 then
  TMoonPascal.FreeLuaLibrary;

end.
