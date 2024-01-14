---
title:    "Swift recipe: Capitalizing a string"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing strings may seem like a simple task, but in Swift, there are multiple ways to do it. Whether you're trying to make your text more visually appealing or following specific formatting guidelines, knowing how to capitalize strings can come in handy.

## How To

To capitalize a string in Swift, we can use the built-in `capitalized` property. It takes into account sentence and word boundaries, so your text will be correctly capitalized regardless of its original case. Let's take a look at an example:

```Swift
let message = "hello, world!"
print(message.capitalized)

// Output: Hello, World!
```

As you can see, the string "hello, world!" has been correctly capitalized using this property.

Another way to capitalize strings is by using the `uppercased` method. This method transforms all characters in a string to uppercase, regardless of their original case. Here's an example:

```Swift
let message = "welcome to my blog"
print(message.uppercased())

// Output: WELCOME TO MY BLOG
```

On the other hand, if you only want to capitalize the first letter of a string, you can use the `prefix(1)` and `lowercased()` methods together. The `prefix(1)` method selects the first letter of the string, and the `lowercased()` method converts it to lowercase. Here's an example:

```Swift
let message = "programming is fun"
print(message.prefix(1).lowercased() + message.dropFirst())

// Output: Programming is fun
```

## Deep Dive

Behind the scenes, the `capitalized` property uses the `capitalizedString(with:)` method, which takes in a `Locale` parameter. This allows the method to take into account different languages and their specific capitalization rules. For example, in German, the `ß` character is usually capitalized to `SS`. The `capitalized` property also handles this rule, so it's a convenient way to capitalize strings for different languages in your app.

## See Also

- [Apple documentation on `capitalized` property](https://developer.apple.com/documentation/foundation/NSString/1418215-capitalized)
- [Apple documentation on `uppercased` method](https://developer.apple.com/documentation/swift/string/2995073-uppercased)
- [Apple documentation on `prefix(_:)` method](https://developer.apple.com/documentation/swift/string/2995048-prefix)