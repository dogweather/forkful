---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Kasowanie znaków pasujących do wzorca to operacja, która pozwala usunąć konkretne znaki z ciągu znaków (stringa). Programiści robią to, aby uporządkować dane wejściowe lub usunąć niepotrzebne informacje.

## Jak to zrobić?

Aby usunąć konkretne znaki pasujące do wzorca, możemy użyć metody `replaceAll()` z klasy String w Java.

```Java
public class Main {
    public static void main(String[] args) {
        String text = "To jest przykładowy tekst do usunięcia.";
        // usuwamy wszystkie wystąpienia litery 'e'
        String modifiedText = text.replaceAll("e", "");
        System.out.println(modifiedText);
    }
}
```

Wynik:

```bash
To jst przykładowy tkst do usunięcia.
```

## Pogłębione informacje

Operacja usuwania znaków pasujących do wzorca pojawiła się już we wczesnych językach programowania i jest ona często wykorzystywana do czyszczenia danych. Alternatywami dla metody `replaceAll()` mogą być metody `replace()` i `replaceFirst()`, które również pozwalają na usuwanie konkretnej sekwencji znaków. W kontekście implementacji, metoda `replaceAll()` w Javie korzysta z mechanizmu wyrażeń regularnych, co umożliwia usuwanie bardziej skomplikowanych wzorców znaków.

## Zobacz także

1. Dokumentacja klasy String w Java: https://docs.oracle.com/javase/10/docs/api/java/lang/String.html
2. Przewodnik po wyrażeniach regularnych w Java: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html
3. Powiązane pytanie na StackOverFlow: https://stackoverflow.com/questions/8726648/remove-specific-char-from-a-string-in-java