# Things-AppContainer-Knows

Here is my collection of tricks that allow a program to retrieve peculiar details about the system even from a restricted environment of a low-privileged AppContainer.

### Features
 - [x] Listing all processes (PID, image name, file location)
 - [x] Listing threads per-process (TID, GUI flag)
 - [ ] Listing loaded modules per-process (filename, sometimes base address, might be incomplete)
 - [ ] Listing services within each svchost process

See the [releases](https://github.com/diversenok/Things-AppContainer-Knows/releases) page to experiment with it yourself.

### Screenshots
Here you can see a complete list of processes on the system from a low-privileged AppContainer sandbox. For every process you can also list all of its threads.

![](https://user-images.githubusercontent.com/30962924/91478606-75fabf00-e8a0-11ea-8b2c-8e50fcf8543e.png)
