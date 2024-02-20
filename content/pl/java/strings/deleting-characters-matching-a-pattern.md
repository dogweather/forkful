---
date: 2024-01-20 17:42:19.378194-07:00
description: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca to filtracja string\xF3\
  w, \u017Ceby pasowa\u0142y do naszych danych lub by\u0142y praktyczne. Robimy to,\
  \ by oczy\u015Bci\u0107 tekst z\u2026"
lastmod: 2024-02-19 22:04:54.389433
model: gpt-4-1106-preview
summary: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca to filtracja string\xF3w, \u017C\
  eby pasowa\u0142y do naszych danych lub by\u0142y praktyczne. Robimy to, by oczy\u015B\
  ci\u0107 tekst z\u2026"
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Usuwanie znaków pasujących do wzorca to filtracja stringów, żeby pasowały do naszych danych lub były praktyczne. Robimy to, by oczyścić tekst z niepotrzebnych treści, jak znaki specjalne czy białe znaki.

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
