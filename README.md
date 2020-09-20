## NAME

`inhibit-screensaver` - inhibit the screen saver if a command exits successfully

## SYNOPSIS

```
Usage: inhibit-screensaver [-f|--frequency SEC] [-a|--activate]
                           [-q|--query SCRIPT] CMD [ARG]
  Inhibit the screensaver if a command exits cleanly.

Available options:
  -f,--frequency SEC       Run CMD every SEC seconds (default: 300)
  -a,--activate            Activate the screensaver if CMD fails
  -q,--query SCRIPT        Run SCRIPT to test if screensaver is currently active
  CMD                      The command to execute
  ARG                      Arguments to pass to CMD
  -h,--help                Show this help text
```

## DESCRIPTION

`inhibit-screensaver` executes a command at regular intervals.  If
the command exists successfully `inhibit-screensaver` will acquire
an inhibit cookie to keep the screen saver from activating.

Since inhibiting the screensaver on Linux is a complete mess, the
following techniques are used:

  * Use the (draft) DBus interface `org.freedesktop.ScreenSaver` to
    acquire and release inhibit locks.

  * Execute `xset` to disable/enable the X screensaver.

## OPTIONS

`-f,--frequency SEC`

  : Controls how often `CMD` should be run.

`-a,--activate`

  : Automatically activate the screensaver/screen lock when `CMD` fails.

    When using this option it is highly recommended that you also use
    the `--query` option, otherwise the screensaver will be
    continually activated and the monitor may never turn off.

    The following methods are used to activate the screensaver:

      * Use `xset` to activate the X screensaver.

      * Use `loginctl lock-session` to lock the current user session.

`-q,--query SCRIPT`

  : Provide a way to test if the screensaver is currently active.

    This allows `inhibit-screensaver` to be smarter about when to run
    `CMD` and when to start the screensaver (if `--activate` is
    given).

    `SCRIPT` is passed to `sh -c` and should exit with success if the
    screensaver is currently active, otherwise it should exit with a
    failure code.

    Example: `--query 'pgrep i3lock'`
