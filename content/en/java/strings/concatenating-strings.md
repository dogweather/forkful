---
title:                "Concatenating strings"
aliases:
- en/java/concatenating-strings.md
date:                  2024-01-20T17:34:55.675076-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings means sticking them together end-to-end to make a new string. It's handy for creating custom messages, building up text for output, or processing user input.

## How to:

Here's the down-and-dirty on how to concatenate strings in Java:

```java
public class StringConcatenationDemo {
    public static void main(String[] args) {
        String firstName = "John";
        String lastName = "Doe";
        
        // Using the plus operator
        String fullName = firstName + " " + lastName;
        System.out.println(fullName); // Output: John Doe
        
        // Using the concat() method
        String anotherFullName = firstName.concat(" ").concat(lastName);
        System.out.println(anotherFullName); // Output: John Doe
        
        // Using StringBuilder for multiple concatenations
        StringBuilder builder = new StringBuilder();
        builder.append(firstName).append(" ").append(lastName);
        System.out.println(builder.toString()); // Output: John Doe
    }
}
```

## Deep Dive

Concatenating strings seems simple enough, right? It's been in Java since the beginning, and we've got a few ways to do it. Early Java versions used StringBuilder under the hood whenever you did a simple `+`. Then came Java 5, and things got more efficient with the introduction of the `StringJoiner` and more improvements to the `StringBuilder` class. 

Now, you might wonder why not always use the `+` operator if it's the same thing? Turns out, `+` is great for a quick job with small strings or a few concatenations. Behind the scenes, though, it can get costly with performance if you're using it in a loop with a lot of iterations because it creates temporary objects before reaching the final string version.

In those heavy-duty cases, enter `StringBuilder` or `StringBuffer`. `StringBuilder` is typically faster because it’s not synchronized—making it thread-unsafe but quick. `StringBuffer` is the older, thread-safe option. It's slower due to synchronization overhead. Choose based on your thread safety needs.

For the `concat()` method, it's straightforward but not as flexible as `StringBuilder`. Want to loop and keep adding more strings? `concat()` is less convenient.

As of Java 8 and beyond, we also have `String.join()` which is pretty neat for joining collections of strings with a delimiter.

## See Also

- [The `String` class documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [The `StringBuilder` class documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuilder.html)
- [The `StringBuffer` class documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuffer.html)
- [Oracle's Java tutorials on Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
