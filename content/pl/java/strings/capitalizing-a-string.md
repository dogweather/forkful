---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:50.938122-07:00
description: "Jak to zrobi\u0107: Standardowa biblioteka Java nie oferuje bezpo\u015B\
  redniej metody na kapitalizacj\u0119 ca\u0142ych ci\u0105g\xF3w za jednym razem,\
  \ ale mo\u017Cna to osi\u0105gn\u0105\u0107, \u0142\u0105cz\u0105c\u2026"
lastmod: '2024-03-13T22:44:35.260105-06:00'
model: gpt-4-0125-preview
summary: "Standardowa biblioteka Java nie oferuje bezpo\u015Bredniej metody na kapitalizacj\u0119\
  \ ca\u0142ych ci\u0105g\xF3w za jednym razem, ale mo\u017Cna to osi\u0105gn\u0105\
  \u0107, \u0142\u0105cz\u0105c wbudowane metody."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

## Jak to zrobić:
Standardowa biblioteka Java nie oferuje bezpośredniej metody na kapitalizację całych ciągów za jednym razem, ale można to osiągnąć, łącząc wbudowane metody. Dla bardziej zaawansowanych potrzeb, biblioteki stron trzecich takie jak Apache Commons Lang oferują proste rozwiązania.

### Używając wbudowanych metod Javy
Aby skapitalizować ciąg bez zewnętrznych bibliotek, możesz podzielić ciąg na słowa, skapitalizować pierwszą literę każdego z nich, a następnie połączyć je z powrotem. Oto proste podejście:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // Wyświetla: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

Ten fragment kodu konwertuje cały ciąg na małe litery, a następnie iteruje przez każdy znak, kapitalizując pierwszą literę każdego słowa. Uznaje spacje, kropki i apostrofy za separator słów.

### Używając Apache Commons Lang
Biblioteka Apache Commons Lang oferuje bardziej eleganckie rozwiązanie za pomocą metody `WordUtils.capitalizeFully()`, która obsługuje dla ciebie różne przypadki brzegowe i separatory:

```java
// Dodaj zależność: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // Wyświetla: "Hello, World!"
    }
}
```

Aby użyć tej metody, musisz dodać bibliotekę Apache Commons Lang do swojego projektu. Ta metoda biblioteczna nie tylko kapitalizuje pierwszą literę każdego słowa, ale także konwertuje resztę liter w każdym słowie na małe litery, zapewniając spójny wzór kapitalizacji w całym ciągu.
