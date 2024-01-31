---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"

category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Zmiana na wielkie litery (ang. capitalizing) to przekształcenie pierwszej litery każdego słowa w łańcuchu znaków na wielką. Robimy to dla poprawy estetyki tekstu, nagłówków lub do zastosowań, gdzie konwencja wymaga użycia wielkich liter, jak w tytułach.

## How to: (Jak to zrobić:)
```java
public class CapitalizeExample {
    public static void main(String[] args) {
        String text = "tu jest przykład tekstu";
        String capitalizedText = capitalizeString(text);
        System.out.println(capitalizedText); // Wyjście: Tu Jest Przykład Tekstu
    }

    public static String capitalizeString(String str) {
        String words[] = str.split("\\s");
        StringBuilder capitalizedStr = new StringBuilder();

        for(String word : words){
            String firstLetter = word.substring(0, 1).toUpperCase();
            String remainingLetters = word.substring(1);
            capitalizedStr.append(firstLetter).append(remainingLetters).append(" ");
        }

        return capitalizedStr.toString().trim();
    }
}
```

## Deep Dive (Zagłębienie się)
Kapitalizacja łańcuchów znaków ma długi rodowód, sięgający maszyn do pisania i wczesnych systemów komputerowych, gdzie wszystkie litery były wielkie z braku innych opcji. Współcześnie mamy biblioteki i metody wbudowane w języki, jak `toUpperCase()` w Javie, jednak one zmieniają wszystkie litery na wielkie, a nie tylko pierwsze w słowie.

Alternatywami są wykorzystanie `StringTokenizer`, biblioteki Apache `StringUtils.capitalize` lub `WordUtils.capitalizeFully`, a także wyrażeń regularnych. Każda metoda ma swoje plusy i minusy, dotyczące czytelności kodu i wydajności. Na przykład, wyrażenia regularne mogą być trudne do zrozumienia, ale świetne, gdy zależy nam na szybkości.

Implementacja zakłada, że tekst jest rozdzielony białymi znakami (spacjami, tabulatorami itd.), co jest typowym rozróżnikiem słów. Warto pamiętać, że metoda `split("\\s")` nie uwzględni kolejnych spacji i może nie działać poprawnie z niektórymi znakami diakrytycznymi we wszystkich kulturach.

## See Also (Zobacz także)
- Java String Documentation: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- Apache Commons Lang StringUtils: https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html
- Regular Expressions in Java: https://docs.oracle.com/javase/tutorial/essential/regex/
