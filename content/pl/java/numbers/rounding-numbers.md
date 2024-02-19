---
aliases:
- /pl/java/rounding-numbers/
date: 2024-01-26 03:46:12.526973-07:00
description: "Zaokr\u0105glanie liczb oznacza dostosowanie ich do okre\u015Blonego\
  \ stopnia precyzji. Programi\u015Bci robi\u0105 to, aby upro\u015Bci\u0107 liczby\
  \ dla czytelno\u015Bci, spe\u0142ni\u0107 okre\u015Blone\u2026"
lastmod: 2024-02-18 23:08:49.472835
model: gpt-4-0125-preview
summary: "Zaokr\u0105glanie liczb oznacza dostosowanie ich do okre\u015Blonego stopnia\
  \ precyzji. Programi\u015Bci robi\u0105 to, aby upro\u015Bci\u0107 liczby dla czytelno\u015B\
  ci, spe\u0142ni\u0107 okre\u015Blone\u2026"
title: "Zaokr\u0105glanie liczb"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaokrąglanie liczb oznacza dostosowanie ich do określonego stopnia precyzji. Programiści robią to, aby uprościć liczby dla czytelności, spełnić określone specyfikacje, lub aby zapewnić, że obliczenia mieszczą się w określonych granicach, jak unikanie błędów precyzji w arytmetyce zmiennoprzecinkowej.

## Jak:
Java oferuje wiele sposobów zaokrąglania liczb. Oto szybka demonstracja z `Math.round()`, `BigDecimal` i `DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Używając Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Wyjście: 123

        // Używając BigDecimal dla większej kontroli
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Wyjście: 123.46

        // Używając DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Wyjście: 123.46
    }
}
```

## Pogłębiona analiza
Historycznie, zaokrąglanie liczb było istotne dla obliczeń analogowych i zostało przeniesione do przetwarzania cyfrowego dla efektywności i dokładności. Błędy zaokrąglania, takie jak te z arytmetyki zmiennoprzecinkowej, pokazują, że to nie jest trywialna kwestia — mogą one kumulacyjnie psuć obliczenia w aplikacjach, powiedzmy, lotniczych i finansowych.

Poza `Math.round()`, masz `BigDecimal`, który daje ci większą kontrolę nad skalą i trybem zaokrąglania, i `DecimalFormat`, gdy potrzebujesz zaokrąglić liczby jako część formatowania wyjścia tekstowego. Alternatywy dla zaokrąglania obejmują zaokrąglanie w dół, zaokrąglanie w górę i obcinanie, które są różnymi sposobami na radzenie sobie z precyzją i zwykle są obsługiwane przez różne metody `Math`.

W zależności od twojego przypadku użycia, strategia zaokrąglania może się różnić. Na przykład, `BigDecimal` jest wyborem dla obliczeń finansowych, gdzie precyzja jest krytyczna. W przeciwieństwie, `Math.round()` jest szybką metodą dla operacji ogólnego przeznaczenia, gdzie nie jesteś tak wymagający co do trybu zaokrąglania.

## Zobacz również
- [Dokumentacja Java Math Oracle'a](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [IEEE Standard dla Arytmetyki Zmiennoprzecinkowej (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [Klasa DecimalFormat w Javie](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
