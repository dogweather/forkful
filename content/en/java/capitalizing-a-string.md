---
title:                "Capitalizing a string"
html_title:           "Java recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizing a String in Java 

## What & Why?

Capitalizing a string means converting the first letter of the string to uppercase. This comes in handy when dealing with titles, names, or anywhere the beginning of a sentence needs emphasis.

## How to:

Here's a nifty way to capitalize a string in Java:

```Java
public class Capitalize {
    public static void main(String[] args) {
        String name = "john doe";
        String capitalizedName = name.substring(0, 1).toUpperCase() + name.substring(1);
        System.out.println(capitalizedName);
    }
}
```

Running the above code will produce:

```Java
John doe
```

What about capitalizing every word in a string, you ask? Easy peasy:

```Java
public class Capitalize {
    public static void main(String[] args) {
        String name = "john doe";
        String[] words = name.split("\\s");
        String capitalizedName = "";
        for (int i = 0; i < words.length; i++){
            capitalizedName += words[i].substring(0, 1).toUpperCase() + words[i].substring(1) + " ";
        }
        System.out.println(capitalizedName.trim());
    }
}
```

This produces:

```Java
John Doe
```

## Deep Dive

Capitalizing strings has been a staple operation since the dawn of computer languages — performing crucial roles in text processing, language localization, and string manipulation tasks.

While Java provides no built-in method to capitalize a string, the community offers a number of solutions, including Apache Commons Lang's `WordUtils.capitalize()`. These libraries can handle complex scenarios out of the box, but for simple capitalizing, the above code snippets do the trick.

Under the hood, Java's `toUpperCase()` uses Unicode data to map lower case characters to their upper case counterparts. If dealing with locale-sensitive text, consider using the overloads that take a `Locale` argument.

## See Also

Interested in diving deeper into Java's string manipulation? These resources are a good place to start:

1. [Java's official String documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)

2. [Apache's Common Lang library](https://commons.apache.org/proper/commons-lang/)

3. [Java's char data type and Unicode](https://docs.oracle.com/javase/tutorial/i18n/text/unicode.html)