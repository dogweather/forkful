---
title:                "Java: Ekstrakcja podciągów"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego?

Wydobycie podłańcuchów jest jedną z podstawowych operacji w dziedzinie przetwarzania tekstu w języku Java. Jest to niezbędne do dokonywania różnych manipulacji na ciągach znaków, takich jak znajdowanie wzorców, zastępowanie lub dzielenie tekstu na mniejsze części. Zrozumienie tej operacji jest niezwykle ważne dla każdego programisty, który zajmuje się analizą lub modyfikacją tekstu w swoich projektach.

## Jak to zrobić?

Aby wydobyć podłańcuch w języku Java, należy skorzystać z metody `substring()`. Przykładowe użycie tej metody wygląda następująco:

```Java
String tekst = "Cześć, jestem programistą Java!";
String podłańcuch = tekst.substring(7, 21); // "jestem programistą"
```

Powyższy kod utworzy nowy ciąg znaków, który jest fragmentem oryginalnego tekstu, zaczynając od 7. znaku i kończąc na 21. znaku (indeksowane od zera). Warto zauważyć, że drugi parametr metody `substring()` jest wyłączony, więc 21. znak nie będzie już częścią wydobytego podłańcucha.

Możliwe jest również podanie tylko jednego parametru metody `substring()`, wtedy będzie ona wydobywać tekst od podanego indeksu do końca:

```Java
String podłańcuch = tekst.substring(7); // "jestem programistą Java!"
```

## Wnikliwy przegląd

Metoda `substring()` nie tylko przyjmuje dwa opcjonalne parametry - początkowy i końcowy indeks, ale również może działać w oparciu o indeksy negatywne. W takim przypadku, indeksy są obliczane od tyłu ciągu znaków, zaczynając od -1. Oznacza to, że ostatni znak ma indeks -1, przedostatni -2, itd. Przykładowo:

```Java
String tekst = "Kot ma mleko!";
String podłańcuch = tekst.substring(4, -1); // "ma mleko"
```

Użycie indeksów negatywnych jest szczególnie przydatne, gdy nie wiemy dokładnie, ile znaków ma nasz tekst lub chcemy wydobyć ostatnią część tekstu.

## Zobacz również

- Dokumentacja Java o metodzie `substring()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-
- Przykładowe użycie `substring()` w przetwarzaniu tekstu: https://howtodoinjava.com/java/string/java-substring-example/