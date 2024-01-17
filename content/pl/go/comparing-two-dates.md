---
title:                "Porównywanie dwóch dat"
html_title:           "Go: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Porównywanie dwóch dat jest procesem polegającym na porównaniu dwóch różnych dat w celu określenia ich wzajemnego stosunku. Programiści często wykonują tę czynność w celu upewnienia się, że data jest odpowiednia lub aby ustalić kolejność wydarzeń w programie.

## Jak to zrobić:
Aby porównać dwie daty w Go, należy użyć funkcji `Equal()` z pakietu `time`. Przykład kodu:

```
data1 := time.Date(2021, time.December, 1, 0, 0, 0, 0, time.UTC)
data2 := time.Date(2020, time.March, 15, 0, 0, 0, 0, time.UTC)
fmt.Println(data1.Equal(data2))
```

Wyjście:

```
false
```

Funkcja `Equal()` zwraca wartość logiczną `true` lub `false` w zależności od tego, czy dwie podane daty są sobie równe czy nie. Do porównania używane są wartości dnia, miesiąca, roku, godziny, minuty, sekundy oraz lokalizacja czasowa.

## Zagłębienie się:
Porównywanie dat jest ważnym aspektem przy tworzeniu programów, szczególnie jeśli pracujemy z czasem i datami. W przeszłości programiści musieli pamiętać o wielu szczegółach, takich jak przeliczanie stref czasowych czy uwzględnianie zmian w kalendarzu gregoriańskim. Dzięki odpowiednim funkcjom w Go, cały ten proces jest prostszy i mniej czasochłonny.

Alternatywnym sposobem porównywania dat jest użycie funkcji `Before()` i `After()` z pakietu `time`, które zwracają wartość logiczną w zależności od kolejności dwóch dat.

W implementacji porównywania dat w Go wykorzystywany jest algorytm równoważny do porównywania wartości liczbowych, co pozwala na szybką i dokładną analizę.

## Zobacz także:
- [Dokumentacja Go - pakiet `time`](https://golang.org/pkg/time/)
- [Porównywanie dat w Java vs. Go](https://plus2net.com/java_tutorial/date-compare-java.php)