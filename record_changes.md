# Changes I have made
## 5-12-25:
- Added maybe_compound_binary_opcode_to_ptr
- Added simple_binary_opcode_to_ptr 
- Used these to decompose and reduce nesting on binary op handling in lower_expr
- Inverted for loop internals in lower_stmt_into DeclStmt handling (early continue)

## 5-19-25
- Added hard-coded make_static array
- Broke decl stmt handling into helper so it's not ultra-nested
- Made should_be_dynamic and make_dynamic_as_needed helpers, injected to decl_stmt handling
- Confirmed that decl_stmt is used in for loops, so no need to duplicate dynamicness logic
- Noted need for support for prefix operators
- Noted need for support for unbraced conditionals


## TODOs:

- actually run buildit, use cute testing script
- make sure that buildit can actually work on generated code
- confirm that Visitor has access to qualified member names
- confirm that member name collisions between structs don't break buildit
- find out how to scope to run only on BSIM
- look into use of bsim in ngspice
- structs don't have constructors, only var_expr


