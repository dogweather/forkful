---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"

category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Регулярні вирази - це шаблони для пошуку та маніпуляції текстом. Програмісти використовують їх для валідації вводу, пошуку чи заміни підрядків.

## How to:
```java
import java.util.regex.*;

public class RegexExample {
    public static void main(String[] args) {
        
        // Example: Validate email address
        String emailRegex = "^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$";
        String emailToCheck = "vash_email@example.com";
        Pattern emailPattern = Pattern.compile(emailRegex);
        Matcher emailMatcher = emailPattern.matcher(emailToCheck);
        boolean isEmailValid = emailMatcher.matches();
        System.out.println("Email is valid: " + isEmailValid);  // Output: Email is valid: true

        // Example: Find and replace all occurrences
        String text = "Котики милі, котики класні, усі люблять котиків.";
        String searchText = "котиків";
        String replaceText = "песиків";
        String replacedText = text.replaceAll(searchText, replaceText);
        System.out.println(replacedText); // Output: Котики милі, котики класні, усі люблять песиків.
    }
}
```

## Deep Dive
Регулярні вирази з'явилися в 1950-х. Головною альтернативою їм є парсери та текстові функції, але вони не такі гнучкі. У Java Pattern і Matcher класи з пакету `java.util.regex` відповідають за роботу з регулярними виразами.

## See Also
- [Java Pattern Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Java Matcher Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [Oracle's regex tutorial](https://docs.oracle.com/javase/tutorial/essential/regex/)
