---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / Що і чому?

Capitalizing a string means turning the first character of each word to uppercase. Programmers often use it to normalize data inputs or enhance readability for users.

## How to: / Як це зробити:

Look at this code. It's simple. We use Java's `StringUtils` from Apache Commons Lang to capitalize words easily. No Apache Commons? No problem. You can also do it with pure Java using `String.split` and a for-loop.

```java
import org.apache.commons.lang3.StringUtils;

public class CapitalizeExample {
    public static void main(String[] args) {
        String message = "вітаю вас у світі java!";
        
        // Using Apache Commons Lang StringUtils
        String capitalizedWithApache = StringUtils.capitalize(message);
        System.out.println(capitalizedWithApache); // Вітаю вас у світі java!

        // Using pure Java
        String[] words = message.split("\\s");
        StringBuilder capitalizedWithJava = new StringBuilder();
        for (String word : words) {
            String firstLetter = word.substring(0, 1).toUpperCase();
            String restOfWord = word.substring(1);
            capitalizedWithJava.append(firstLetter).append(restOfWord).append(" ");
        }
        System.out.println(capitalizedWithJava.toString().trim()); // Вітаю вас у світі java!
    }
}
```

## Deep Dive / Занурення глибше:

Back in the day, methods to capitalize a string were written from scratch. Now, utility libraries like Apache Commons Lang save time. Talking alternatives, there's also Google's Guava `CaseFormat`. Each method has its quirks; for example, `StringUtils` only capitalizes the first character and leaves rest untouched. In custom implementations, you control everything, but it takes more code and adds complexity. Remember, these small details matter for functions like proper name formatting where consistency is key.

## See Also / Дивись також:

To explore more:
- Apache Commons Lang: https://commons.apache.org/proper/commons-lang/
- Google's Guava: https://guava.dev/releases/snapshot/api/docs/com/google/common/base/CaseFormat.html
- JavaDoc for String: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html

These sources offer additional insight and methods that might better suit your use case. Happy coding!
