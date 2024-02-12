---
title:                "Writing to standard error"
aliases:
- /en/cpp/writing-to-standard-error/
date:                  2024-02-03T19:03:33.912340-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing to standard error"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (`stderr`) in C++ involves outputting error messages or diagnostics that are separate from the main program output. Programmers do this to direct errors to a different stream, allowing for easier debugging and error handling by distinguishing normal output from error messages.

## How to:

In C++, writing to standard error can be achieved using the `cerr` stream, which is part of the standard library. Here's a basic example:

```cpp
#include <iostream>

int main() {
    // Writing to standard output
    std::cout << "This is a normal message." << std::endl;
    
    // Writing to standard error
    std::cerr << "This is an error message." << std::endl;
    
    return 0;
}
```

Sample Output:
```
This is a normal message.
This is an error message.
```

In this case, both messages will typically appear on your terminal, but you can redirect them separately in a shell. For instance, you can send standard output to a file while allowing errors to be displayed on the screen.

For more advanced logging and error handling, third-party libraries like `spdlog` or `boost.log` can be employed. These libraries offer enhanced features for logging, including formatting, log levels, and file output.

Here's how you might use `spdlog` to write an error message:

```cpp
#include "spdlog/spdlog.h"

int main() {
    // Initialize spdlog
    spdlog::info("This is a normal message.");
    spdlog::error("This is an error message.");
    
    return 0;
}
```

Note: To use `spdlog`, you need to add it to your project. You can do this by cloning the repository from GitHub or using a package manager like `vcpkg` or `conan`. 

Remember, the choice between using standard streams directly or a library like `spdlog` depends on the complexity of your application and your specific needs regarding error handling and logging.
