# Things-AppContainer-Knows

Here is my collection of tricks that allow a program to retrieve peculiar details about the system even from a restricted environment of a low-privileged AppContainer.

### Features
 - [x] Listing all processes (PID, image name, file location)
 - [ ] Listing all threads per-process (TID only)
 - [ ] Listing loaded modules per-process (filename, sometimes base address, might be incomplete)
 - [ ] Listing services within each svchost process

See the [releases](https://github.com/diversenok/Things-AppContainer-Knows/releases) page to experiment with it yourself.

### Screenshots
Here you can see a complete list of processes on the system from a low-privileged AppContainer sandbox. 

![](https://user-images.githubusercontent.com/30962924/91461377-31aff480-e889-11ea-87b9-7907651da219.png)
