---
title:                "Searching and replacing text"
date:                  2024-01-20T17:58:03.877086-07:00
model:                 gpt-4-1106-preview
simple_title:         "Searching and replacing text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text in Java scribbles over original strings with new characters - think of it as digital white-out. Programmers often use this to clean data, tweak settings, or tailor messages.

## How to:

Searching and replacing in Java is a breeze thanks to the `String` class and its `replace()` method. Here's how you do it:

```java
public class ReplaceDemo {
    public static void main(String[] args) {
        String originalText = "The quick brown fox jumps over the lazy dog";
        String modifiedText = originalText.replace("lazy", "energetic");
        
        System.out.println("Before: " + originalText);
        System.out.println("After: " + modifiedText);
    }
}
```

Output:
```
Before: The quick brown fox jumps over the lazy dog
After: The quick brown fox jumps over the energetic dog
```

Now, for patterns or wilder replacements, `Pattern` and `Matcher` come into play:

```java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class RegexReplaceDemo {
    public static void main(String[] args) {
        String originalText = "There are 31,536,000 seconds in 365 days.";
        Pattern pattern = Pattern.compile("\\d+");
        Matcher matcher = pattern.matcher(originalText);
        String modifiedText = matcher.replaceAll("#");
        
        System.out.println("Before: " + originalText);
        System.out.println("After: " + modifiedText);        
    }
}
```

Output:
```
Before: There are 31,536,000 seconds in 365 days.
After: There are # seconds in # days.
```

## Deep Dive:

The `replace()` method traces its origins to the earliest days of Java. It's part of the immutable `String` class, which means every time you use it, you're creating a new string. Very eco-friendly, no waste of the old stuff.

But what's the deal with `Pattern` and `Matcher`, you ask? These classes are part of Java's regular expression (regex) API, introduced in Java 1.4. They add teeth to search and replace, allowing you to detect complex patterns and modify text dynamically. It's like using a scalpel instead of a sledgehammer.

Plus, there's `replaceAll()` and `replaceFirst()`, two methods of the `Matcher` class that fine-tune your text transformations, replacing all occurrences or just the first match.

Another alternative is using the `StringBuffer` or `StringBuilder` classes when you're dealing with tons of modifications because unlike `String`, these buffers are mutable.

## See Also:

- [Java String Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Java Pattern Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Matcher Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [Regular Expressions Tutorial](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)

For more hands-on practice, check out RegexOne (https://regexone.com), it's a great resource to level up your regex skills.
