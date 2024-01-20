---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? 
Deleting characters that match a pattern in Java is a process in which specific characters from a String are removed based on a defined set of rules. Programmers do this to modify and refine data, conforming to data rules or user requirements.

## How To?

In Java, you can utilize the `replaceAll()` method along with regular expressions (regex) to delete characters that match a pattern. Here's an example:

```Java
public class DeleteCharacter {
    public static void main(String[] args) {
        String hello = "H.e.l.l.o...W.o.r.l.d...";

        String cleanText = hello.replaceAll("\\.", "");

        System.out.println(cleanText);
    }
}
```
In this case, the output will be "HelloWorld". Here `replaceAll("\\.", "")` removes all periods from the string. 

## Deep Dive

The technique of deleting characters from a string using patterns has its roots in the broader concept of pattern matching, and it is widespread in computer science and programming from very early on. 

Alternatives to the `replaceAll()` method are using the `replace()` method if you have a fixed pattern to replace or using `StringUtils` from Apache Commons Lang library.

Inside Java, `replaceAll()` constructs a new String by replacing all the matches of the `regex` in the existing string. It uses `Pattern` and `Matcher` classes under the hood.

## See Also

Refer to Java's official documentation for more on [replaceAll()](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replaceAll(java.lang.String,%20java.lang.String)) or [Pattern class](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html). For more on StringUtils, check out [Apache Commons Lang library](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html).