---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyszukiwanie i zamienianie tekstu to proces, który pozwala odnaleźć konkretne fragmenty tekstu i wymienić je na inne. Programiści robią to przeważnie dla automatyzacji edycji w tekstach o dużym rozmiarze.

## Jak to zrobić:
Oto jak używamy metody `replace()` w Java, aby wyszukać i zamienić tekst:

```Java
public class Main {
    public static void main(String[] args) {
        String txt = "Witaj, świecie!";
        String newTxt = txt.replace("świecie", "Java");

        System.out.println(newTxt);
    }
}
```
Na wyjściu dostaniemy:

```
Witaj, Java!
```

## Zanurzmy się głębiej:
Pierwsze techniki zamiany tekstu pojawiały się w latach 70. z pojawieniem się pierwszych edytorów tekstowych. Alternatywami dla metody `replace()` w Java są skomplikowane metody korzystające z wyrażeń regularnych, takie jak `replaceAll()` lub `replaceFirst()`.

Do implementacji `replace()` w Java używa się algorytmu dwuetapowego. Najpierw przegląda się ciąg znaków, a następnie tworzy nowy ciąg znaków z zamienionym tekstem.

## Zobacz również:
1. [Dokumentacja Oracle för metody replace()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char-)
2. [Tutorial do używania metody replace()](https://www.javatpoint.com/java-string-replace)
3. [Dyskusja o wydajności metody replace()](https://stackoverflow.com/questions/16228992/commons-lang-stringutils-replace-performance-vs-string-replace)
4. [Porównanie replace() i replaceAll()](http://javaconceptoftheday.com/difference-between-replace-and-replaceall-in-java/)