## NAME

`inhibit-screensaver` - inhibit the screen saver if a command exits successfully

## SYNOPSIS

```
Usage: inhibit-screensaver [-f|--frequency SEC] CMD [ARG]
  Inhibit the screensaver if a command exits cleanly.

Available options:
  -f,--frequency SEC       Run CMD every SEC seconds (default: 300)
  CMD                      The command to execute
  ARG                      Arguments to pass to CMD
  -h,--help                Show this help text
```

## DESCRIPTION

`inhibit-screensaver` executes a command at regular intervals.  If
the command exists successfully `inhibit-screensaver` will acquire
an inhibit cookie to keep the screen saver from activating.
