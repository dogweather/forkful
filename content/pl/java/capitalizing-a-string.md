---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Java: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zmiana pierwszej litery w łańcuchu znaków (string) na wielką w języku Java to tzw. *kapitalizacja*. Programiści stosują to do poprawienia formatowania tekstu, na przykład nazw własnych, tytułów książek i artykułów.

## Jak to zrobić?

Metoda na kapitalizację to proste zadanie w Javie. Możemy skorzystać ze wbudowanej metody `substring()`, jak pokazano poniżej:

```Java
public class Main {
    public static void main(String[] args) {

        String tekst = "jestem programistą java";
        String kapitalizowanyTekst = tekst.substring(0, 1).toUpperCase() + tekst.substring(1);

        System.out.println(kapitalizowanyTekst);
    }
}
```

Wynik:

```Java
Jestem programistą java
```

## Przyjrzyjmy się bliżej

Historia back-endu jest pełna sytuacji, w których kapitalizacja łańcuchów była koniecznością. W przeszłości, zanim HTML i CSS pozwoliły na kontrolowanie wielkości liter, zależało to od języka back-endowego. 

Alternatywą do powyższego rozwiązania jest użycie biblioteki Apache Commons Lang, która ma bezpośrednią metodę do kapitalizacji.

```Java
import org.apache.commons.lang3.text.WordUtils;
public class Main {
    public static void main(String[] args) {

        String tekst = "jestem programistą java";
        String kapitalizowanyTekst = WordUtils.capitalize(tekst);

        System.out.println(kapitalizowanyTekst);
    }
}
```

Wynik:

```Java
Jestem Programistą Java
```

Niektóre detale implementacji to, że metoda `substring()` w Javie tworzy nowy łańcuch, co może prowadzić do nieefektywności przy dużej ilości danych.

## Zobacz również

* Dokumentacja Java na temat `substring()`: [https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int)](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int))
* Apache Commons Lang: [https://commons.apache.org/proper/commons-lang/](https://commons.apache.org/proper/commons-lang/)
* Dokumentacja WordUtils.capitalize(): [https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/text/WordUtils.html#capitalize-java.lang.String-](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/text/WordUtils.html#capitalize-java.lang.String-)