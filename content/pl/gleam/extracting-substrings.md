---
title:    "Gleam: Wydobywanie podciągów"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Jesteś nowy w języku programowania Gleam i zastanawiasz się po co służy ekstrakcja podciągów w programowaniu? Nie ma się czego bać, jest to przydatna technika w wielu sytuacjach. Pozwala na wydobycie określonych fragmentów tekstu z większej całości, co ułatwia pracę z danymi w kodzie. W tym artykule dowiesz się, jak wykorzystać ekstrakcję podciągów w Gleam do swoich celów.

## Jak to zrobić

Kodowanie w Gleam może wydawać się trudne na pierwszy rzut oka, ale z ekstrakcją podciągów nie jest tak strasznie. Wystarczy skorzystać z metod `slice` lub `subslice`, które pozwalają na wyciągnięcie odpowiedniej części tekstu według podanych parametrów.

Poniższy przykład kodu demonstruje użycie metody `subslice`, aby wyodrębnić fragment tekstu o indeksach od 3 do 7 (włącznie) ze zmiennej `text`:

```Gleam
let text = "To jest przykładowy tekst"
let substring = text.subslice(3, 7)

gleam_assert_eq(substring, "jest")
```

Jako rezultat otrzymujemy wyodrębniony fragment tekstu "jest". Takie proste wykorzystanie pozwala na dostosowanie naszego kodu do własnych potrzeb.

## Dogłębna analiza

Możliwość ekstrakcji podciągów jest jedną z wielu użytecznych funkcji języka Gleam. Wystarczy jedna linia kodu, by wydobyć odpowiedni fragment tekstu i wykorzystać go w swoim programie. Metody `slice` i `subslice` również pozwalają na ustawienie dodatkowych parametrów, takich jak krok lub indeks początkowy, co daje nam większą kontrolę nad ekstrakcją.

Ekstrakcja podciągów jest szczególnie przydatna, gdy mamy do czynienia z danymi w formie tekstu, np. plikami CSV czy JSON. Dzięki niej możemy w prosty sposób wyodrębnić potrzebne nam informacje z tych plików i wykorzystać je w dalszej części programu.

## Zobacz także

- [Dokumentacja Gleam o metodzie `slice`](https://gleam.run/documentation/0.10.0/std/string.html#slice)
- [Dokumentacja Gleam o metodzie `subslice`](https://gleam.run/documentation/0.10.0/std/string.html#subslice)
- [Gleam - oficjalna strona języka](https://gleam.run/)