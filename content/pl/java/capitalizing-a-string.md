---
title:                "Zmiana tekstu na wielkie litery"
html_title:           "Java: Zmiana tekstu na wielkie litery"
simple_title:         "Zmiana tekstu na wielkie litery"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Podczas programowania często spotykamy się z pojęciem kapitalizacji ciągów znaków. Kapitalizacja oznacza konwersję wszystkich pierwszych liter w łańcuchu znaków na wielkie litery. Programiści stosują tę technikę w celu poprawy czytelności i spójności swojego kodu.

## Jak to zrobić:

```Java
String str = "hello world";
String capitalized = str.toUpperCase();
System.out.println(capitalized);
```
Output: HELLO WORLD

## Zanurz się:

Kapitalizacja ciągów znaków jest powszechnie stosowana w programowaniu, ponieważ ułatwia czytanie i zrozumienie kodu. Alternatywnym sposobem na kapitalizację jest użycie klasy `Character` i jej metody `toUpperCase()`. Implementacja kapitalizacji może być również dostosowywana przez programistów do swoich indywidualnych potrzeb poprzez użycie różnych metod dostępnych w języku Java.

## Zobacz także:

Więcej informacji na temat kapitalizacji ciągów znaków w języku Java można znaleźć w oficjalnej dokumentacji: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--. Istnieje również wiele narzędzi i bibliotek, które można wykorzystać do kapitalizacji tych ciągów. Przykładem jest biblioteka Apache Commons Text: https://commons.apache.org/proper/commons-text/apidocs/org/apache/commons/text/WordUtils.html#capitalize-java.lang.String...