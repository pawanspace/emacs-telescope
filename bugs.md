# Known Bugs

## Fixed
- ✅ `recenter` error in preview buffer - fixed by proper window/buffer context nesting
- ✅ Preview not scrolling to matched line in grep results - fixed buffer/window context in recenter calls
- ✅ Read-only buffer error on subsequent grep calls - buffers now killed and recreated fresh
- ✅ Results buffer scrolling to end on navigation - added proper point positioning
- ✅ Navigation keybindings - added arrow keys (<up>/<down>) which work reliably

## Open
None currently
