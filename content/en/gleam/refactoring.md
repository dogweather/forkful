---
title:                "Refactoring"
date:                  2024-01-25T02:12:45.914971-07:00
model:                 gpt-4-1106-preview
simple_title:         "Refactoring"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/refactoring.md"
---

{{< edit_this_page >}}

## What & Why?
Refactoring is the process of reworking your code to make it cleaner, more maintainable, without altering its external behavior. Programmers refactor to improve readability, reduce complexity, and to make the codebase more amenable to future updates or feature additions.

## How to:
Let's say you've got a chunk of code where you're doing some repeated calculations or string manipulations across multiple functions. That's a prime target for refactoring. Here's a before-and-after using Gleam, which has a strong emphasis on type safety and immutability:

```gleam
// Before refactoring
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("The area is \(area)")
}

// After refactoring
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("The area is \(area)")
}

// In another part of your code, you'll call print_area like this:
print_area(calculate_area(10, 20))
```

Sample output:
```
The area is 200
```

By refactoring, we've made `print_area` more focused on just printing, while the calculation is handled elsewhere, making the code more modular and easier to reuse or test.

## Deep Dive
Refactoring, as a concept, has been around as long as programming itself—revisiting and cleaning up code is part of good housekeeping. The modern formalization of refactoring, along with many of the techniques and patterns used today, can be traced back to Martin Fowler's seminal book "Refactoring: Improving the Design of Existing Code" published in 1999.

In the Gleam ecosystem, refactoring has specific considerations. One of the most significant is the strong type checking at compile time, which can help catch mistakes early when you're moving things around. Gleam's pattern matching and immutability features can also guide you to write clearer, more concise code—one of the primary goals of refactoring.

Alternatives to refactoring might include rewriting code from scratch or patching code with quick fixes. Refactoring, however, is usually the safest and most efficient approach to improving existing code without introducing new bugs, as it involves incremental, well-underlined, behavior-preserving transformations.

## See Also
- Martin Fowler's "Refactoring" book: https://martinfowler.com/books/refactoring.html
- The Gleam language website, with additional documentation and examples: https://gleam.run/
- "Refactoring: Improving the Design of Existing Code" by Martin Fowler (for underlying principles applicable across languages): https://martinfowler.com/books/refactoring.html