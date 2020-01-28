% -----------------------------------------------------------------------------
% Macro for logging. 
% -----------------------------------------------------------------------------
-ifndef(LOG_HRL).
-define(LOG_HRL, true).
% -----------------------------------------------------------------------------

-define(DEBUG(Fmt), lager:debug(Fmt)).
-define(DEBUG(Fmt, Args), lager:debug(Fmt, Args)).

-define(INFO(Fmt), lager:info(Fmt)).
-define(INFO(Fmt, Args), lager:info(Fmt, Args)).

-define(NOTICE(Fmt), lager:notice(Fmt)).
-define(NOTICE(Fmt, Args), lager:notice(Fmt, Args)).

-define(WARNING(Fmt), lager:warning(Fmt)).
-define(WARNING(Fmt, Args), lager:warning(Fmt, Args)).

-define(ERROR(Fmt), lager:error(Fmt)).
-define(ERROR(Fmt, Args), lager:error(Fmt, Args)).

-define(CRITICAL(Fmt), lager:critical(Fmt)).
-define(CRITICAL(Fmt, Args), lager:critical(Fmt, Args)).

-define(ALERT(Fmt), lager:alert(Fmt)).
-define(ALERT(Fmt, Args), lager:alert(Fmt, Args)).

-define(EMERGENCY(Fmt), lager:emergency(Fmt)).
-define(EMERGENCY(Fmt, Args), lager:emergency(Fmt, Args)).

% -----------------------------------------------------------------------------

-define(PRECORD(R), lager:pr(R, ?MODULE)).

-define(M,     ?MODULE).
-define(M_F,   {?MODULE, ?FUNCTION_NAME}).
-define(M_F_L, {?MODULE, ?FUNCTION_NAME, ?LINE}).

-define(M(N),     {?MODULE, N}).
-define(M_F(N),   {?MODULE, N, ?FUNCTION_NAME}).
-define(M_F_L(N), {?MODULE, N, ?FUNCTION_NAME, ?LINE}).

% -----------------------------------------------------------------------------
-endif.
% -----------------------------------------------------------------------------
