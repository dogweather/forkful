---
title:                "Clojure: Wycinanie podciągów"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyciąganie podciągów (lub części ciągów) jest nieodłączną częścią wielu zadań programistycznych. Może to być konieczne, gdy chcemy przetworzyć dłuższe ciągi znaków, aby uzyskać tylko pewne wybrane wartości. Na przykład, gdy chcemy wyodrębnić numer telefonu lub adres e-mail z dłuższego tekstu. Clojure posiada wiele funkcji, które ułatwiają nam to zadanie.

## Jak to zrobić

Aby wyodrębnić podciągi w Clojure, musimy skorzystać z funkcji `subs` lub `substring`. Obie funkcje przyjmują dwa argumenty: ciąg znaków oraz indeksy określające początek i koniec wyciąganego podciągu. Przykładowo, gdy chcemy uzyskać podciąg z ciągu "Hello World!" zaczynający się od indeksu 2 i kończący na indeksie 6, musimy wpisać:

```Clojure
(subs "Hello World!" 2 6)
```

Dzięki temu otrzymamy wartość "llo W".

Możemy również wykorzystać funkcję `split` do rozdzielenia ciągu na podciągi za pomocą określonego separatora. Na przykład, gdy chcemy rozdzielić adres e-mail na część przed "@" i część po "@", możemy wykorzystać funkcję `split`, wpisując:

```Clojure
(split "example@example.com" #"\@")
```

Jako oddzielacz używamy tu wyrażenia regularnego, które jest ujęte w "#". Dzięki temu otrzymamy dwa podciągi: "example" oraz "example.com".

## Głębszy zanurzenie

Funkcje `subs` i `substring` są bardzo podobne, jednak różnią się w kwestii indeksów. Funkcja `subs` przyjmuje indeksy początkowe i końcowe włącznie, natomiast funkcja `substring` przyjmuje indeks początkowy włącznie, a indeks końcowy jest wyłączony. Może to wprowadzać nieco nieoczekiwane wyniki, dlatego ważne jest zawsze uważnie czytać dokumentację funkcji.

Clojure posiada także wiele innych funkcji do operacji na ciągach, takich jak `take` czy `drop`, które mogą również być wykorzystane do wyodrębniania podciągów.

## Zobacz również

- Dokumentacja funkcji `subs` i `substring`: https://clojuredocs.org/clojure.core/substring
- Dokumentacja funkcji `split`: https://clojuredocs.org/clojure.string/split
- Więcej funkcji do operacji na ciągach w Clojure: https://www.braveclojure.com/core-functions-in-depth/#Functions_for_Working_with_Clojure_St