#IfNDef LIBTCC_BI
#Define LIBTCC_BI

#IfNDef LIBTCCAPI
 #Define LIBTCCAPI
#EndIf

Extern "C" Lib "tcc"

Type TCCState As Any

/' create a new TCC compilation context '/
Declare Function tcc_new LIBTCCAPI() As TCCState Ptr

/' free a TCC compilation context '/
Declare Sub tcc_delete LIBTCCAPI(s As TCCState Ptr)

/' set CONFIG_TCCDIR at runtime '/
Declare Sub tcc_set_lib_path LIBTCCAPI(s As TCCState Ptr, path As Const ZString Ptr)

/' set error/warning display callback '/
Type tcc_error_proc As Sub CDecl (opaque As Any Ptr, msg As Const ZString Ptr)
Declare Sub tcc_set_error_func LIBTCCAPI(s As TCCState Ptr, error_opaque As Any Ptr, error_sub As tcc_error_proc)

/' set options as from command line (multiple supported) '/
Declare Function tcc_set_options LIBTCCAPI(s As TCCState Ptr, str_ As Const ZString Ptr) As Long

'/'''''''''''''''''''''''''''''/
/' preprocessor '/

/' add include path '/
Declare Function tcc_add_include_path LIBTCCAPI(s As TCCState Ptr, pathname As Const ZString Ptr) As Long

/' add in system include path '/
Declare Function tcc_add_sysinclude_path LIBTCCAPI(s As TCCState Ptr, pathname As Const ZString Ptr) As Long

'/' define preprocessor symbol 'sym'. Can put optional value '/
Declare Sub tcc_define_symbol LIBTCCAPI(s As TCCState Ptr, sym As Const ZString Ptr, value As Const ZString Ptr)

'/' undefine preprocess symbol 'sym' '/
Declare Sub tcc_undefine_symbol LIBTCCAPI(s As TCCState Ptr, sym As Const ZString Ptr)

'/'''''''''''''''''''''''''''''/
/' compiling '/

/' add a file (C file, dll, object, library, ld script). Return -1 if error. '/
Declare Function tcc_add_file LIBTCCAPI(s As TCCState Ptr, filename As Const ZString Ptr) As Long

/' compile a string containing a C source. Return -1 if error. '/
Declare Function tcc_compile_string LIBTCCAPI(s As TCCState Ptr, buf As Const ZString Ptr) As Long

'/'''''''''''''''''''''''''''''/
/' linking commands '/

/' set output type. MUST BE CALLED before any compilation '/
Declare Function tcc_set_output_type LIBTCCAPI(s As TCCState Ptr, output_type As Long) As Long
#Define TCC_OUTPUT_MEMORY   0 /' output will be run in memory (default) '/
#Define TCC_OUTPUT_EXE      1 /' executable file '/
#Define TCC_OUTPUT_DLL      2 /' dynamic library '/
#Define TCC_OUTPUT_OBJ      3 /' object file '/
#Define TCC_OUTPUT_PREPROCESS 4 /' only preprocess (used internally) '/

/' equivalent to -Lpath option '/
Declare Function tcc_add_library_path LIBTCCAPI(s As TCCState Ptr, pathname As Const ZString Ptr) As Long

'/' the library name is the same as the argument of the '-l' option '/
Declare Function tcc_add_library LIBTCCAPI(s As TCCState Ptr, libraryname As Const ZString Ptr) As Long

/' add a symbol to the compiled program '/
Declare Function tcc_add_symbol LIBTCCAPI(s As TCCState Ptr, name_ As Const ZString Ptr, val_ As Const Any Ptr) As Long

/' output an executable, library or object file. DO NOT call
   tcc_relocate() before. '/
Declare Function tcc_output_file LIBTCCAPI(s As TCCState Ptr, filename As Const ZString Ptr) As Long

/' link and run main() function and return its value. DO NOT call
   tcc_relocate() before. '/
Declare Function tcc_run LIBTCCAPI(s As TCCState Ptr, argc As Long, argv As ZString Ptr Ptr) As Long

/' do all relocations (needed before using tcc_get_symbol()) '/
Declare Function tcc_relocate LIBTCCAPI(s1 As TCCState Ptr, ptr_ As Any Ptr) As Long
/' possible values for 'ptr':
   - TCC_RELOCATE_AUTO : Allocate and manage memory internally
   - NULL              : return required memory size for the step below
   - memory address    : copy code to memory passed by the caller
   returns -1 if error. '/
#Define TCC_RELOCATE_AUTO CPtr(Any Ptr, 1)

/' return symbol value or NULL if not found '/
Declare Function tcc_get_symbol LIBTCCAPI(s As TCCState Ptr, name_ As Const ZString Ptr) As Any Ptr

End Extern

#EndIf 'LIBTCC_BI