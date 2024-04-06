---
date: 2024-01-20 17:42:19.378194-07:00
description: "Jak to zrobi\u0107: Usuwanie znak\xF3w pasuj\u0105cych do wzorca si\u0119\
  ga pocz\u0105tk\xF3w programowania, gdzie oszcz\u0119dno\u015B\u0107 pami\u0119\
  ci by\u0142a kluczowa. J\u0119zyk Java u\u0142atwia to zadanie\u2026"
lastmod: '2024-04-05T21:53:36.701408-06:00'
model: gpt-4-1106-preview
summary: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca si\u0119ga pocz\u0105tk\xF3\
  w programowania, gdzie oszcz\u0119dno\u015B\u0107 pami\u0119ci by\u0142a kluczowa."
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## Jak to zrobić:
```java
import java.util.regex.Pattern;

public class PatternDeletionDemo {
    public static void main(String[] args) {
        String text = "Jabłka, gruszki & 33 banany";
        String pattern = "[^\\w\\s]+"; // Wzorzec do usunięcia wszystkiego oprócz liter, cyfr i białych znaków
        
        String cleanedText = text.replaceAll(pattern, "");
        System.out.println(cleanedText); // Wyświetla: Jabłka gruszki  33 banany
    }
}
```

## Deep Dive
Usuwanie znaków pasujących do wzorca sięga początków programowania, gdzie oszczędność pamięci była kluczowa. Język Java ułatwia to zadanie dzięki klasie `Pattern` z pakietu `java.util.regex`, która implementuje wyrażenia regularne. Alternatywy to inne języki jak Perl, znany z potężnego systemu wyrażeń regularnych. Dodatkowo, mamy metody jak `String.replace()` dla prostych podmian bez wzorców. Pamiętajmy, że kompilacja wzorca może być kosztowna. W takim przypadku użycie `Pattern.compile()` do wielokrotnego wykorzystania wzorca jest efektywniejsze.

## Zobacz też
- Dokumentacja klasy `Pattern` w Java: [https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html)
- Poradnik Oracle o wyrażeniach regularnych: [https://docs.oracle.com/javase/tutorial/essential/regex/](https://docs.oracle.com/javase/tutorial/essential/regex/)
- Strona do testowania wyrażeń regularnych: [https://regexr.com/](https://regexr.com/)
