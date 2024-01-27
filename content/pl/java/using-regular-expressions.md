---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
W Java wyrażenia regularne to sposób wyszukiwania i manipulowania tekstami. Programiści używają ich, by szybko znajdować wzorce i pracować z danymi tekstu - sprawdzanie poprawności, wycinanie fragmentów, zamiany.

## How to:
```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExamples {
    public static void main(String[] args) {
        // Wyszukiwanie wzorca "java"
        String text = "java jest fajna, Java jest potężna!";
        Pattern pattern = Pattern.compile("java", Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);

        while (matcher.find()) {
            System.out.println("Znaleziono: " + matcher.group());
        }

        // Zamiana wszystkich wystąpień "java" na "JAVA"
        String replacedText = matcher.replaceAll("JAVA");
        System.out.println(replacedText);
    }
}
```
Sample output:
```
Znaleziono: java
Znaleziono: Java
java jest fajna, JAVA jest potężna!
```

## Deep Dive:
Wyrażenia regularne powstały w latach 50. XX wieku. Są alternatywy jak parsowanie tekstu, ale wyrażenia regularne nadal popularne ze względu na uniwersalność. W Java, `Pattern` i `Matcher` klasy z pakietu `java.util.regex` to kluczowe elementy do pracy z wyrażeniami regularnymi.

## See Also:
- Dokumentacja Java Pattern Class: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- Java Regex Tester - narzędzie online do testowania Twoich wyrażeń regularnych: https://www.freeformatter.com/java-regex-tester.html
- Tutorial Oracle o wyrażeniach regularnych: https://docs.oracle.com/javase/tutorial/essential/regex/
