# eknife - Erlang Swiss Army Knife 

Set of tools for building erlang applications.

Consist of:
* Macros for logger, supervisor and timer;
* Pub/Sub simpler interface (based on gproc); 
* Images type detect and check size (jpeg only);
* Rest clients (based on gun);
* Some utilites:
  * stringify stacktrace (using in exception);
  * type cast functions;
  * date/time operations;
  * json operations;
  * hash operations.
  
## Usage  

Put 
```
    {eknife,  {git, "https://github.com/relabsoss/eknife.git", {branch, "master"}}}
```
or (for OTP 27 or higher)
```
    {eknife,  {git, "https://github.com/relabsoss/eknife.git", {branch, "otp27"}}}
```
to your rebar.config

and
```
-include_lib("eknife/include/eknife.hrl").
```
to common include file.

The code is too small for per line documentation. Check it by yourself.
