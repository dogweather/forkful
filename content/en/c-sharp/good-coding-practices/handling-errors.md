---
title:                "Handling errors"
aliases:
- /en/c-sharp/handling-errors.md
date:                  2024-01-21T21:19:09.226800-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?

Handling errors in C# is about managing the unexpected—like tripping over your shoelaces. Programs can trip over bad data or wonky connections. We handle errors to keep our software from face-planting, letting it recover gracefully.

## How to:

Let's start with a try-catch block. It's like putting a safety net under a tightrope walker. If they slip, they don’t plummet—they're caught.

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] numbers = {1, 2, 3};
            Console.WriteLine(numbers[5]);  // Oops, index is out of bounds!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Caught an error: " + e.Message);
        }
    }
}
```

Sample output when things go south:
```
Caught an error: Index was outside the bounds of the array.
```

Now we add a finally block—it’s what happens no matter what, like paying taxes.

```C#
try {
    // Potentially troublesome code here
} catch (SomeSpecificException e) {
    // Handle that specific error here
} finally {
    // This code runs no matter what happens above
    Console.WriteLine("This always runs.");
}
```

## Deep Dive

Error handling's been in C# since its birth. Over time, it's evolved. Back in the day, programmers relied on return codes or global flags to signal problems—clunky and error-prone.

C# uses exceptions, a more modern approach. An exception is thrown when the unexpected happens, just like throwing a flag on the play in football. Structured exception handling with try, catch, and finally blocks makes managing these moments clearer and cleaner than old-school error checking.

Alternatives? Sure. There's the `UnhandledExceptionEventHandler` for exceptions that slip through. Or in async code, error handling turns a bit on its head with `Task` objects that carry their own baggage of exceptions.

Implementation details—akin to the fine print—matter. Exceptions can be costly, dragging down performance if thrown willy-nilly. So, we use them for exceptional cases, not everyday logic control.

## See Also

- [Official documentation on Exceptions in C#](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Best practices in C# exception handling](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/best-practices-for-exceptions)
