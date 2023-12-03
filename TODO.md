# TODO

- [ ] Get state and event code in working order
- [ ] Write tests
- [ ] Get TUI in working order (brick package) and responding to Reflex events
- [ ] Use ptrace to monitor running portage processes to get their exit code?
      (not sure how useful this will actually be, but in some situations it
      could let us know if the build completed successfully or not)

## Interesting haskell packages:

- `linux-ptrace`: Wrapping of Linux' ptrace(2).
- `proc`: Parse process information for Linux
- `unix`: System.Posix.User (getEffectiveUserName (for checking for root))
