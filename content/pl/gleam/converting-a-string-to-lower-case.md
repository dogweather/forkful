---
title:    "Gleam: Konwersja ciągu znaków na małe litery"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów często spotyka się z koniecznością zmiany tekstu na małe litery w swoim kodzie. Czasami jest to potrzebne do porównania dwóch napisów lub prostego przetwarzania danych. W tym artykule omówimy, jak to zrobić w języku programowania Gleam.

## Jak to zrobić

Aby zamienić tekst na małe litery w języku Gleam, musimy użyć funkcji ```String.to_lower``` i przekazać jako argument nasz napis. Oto przykładowy kod:

```Gleam
let text = "To Jest Przykładowy Tekst"
let lowercase_text = String.to_lower(text)
```

Wywołanie tej funkcji spowoduje zamianę tekstu na "to jest przykładowy tekst". Jeśli chcemy przekonwertować tekst bez zapisywania go w zmiennej, możemy to zrobić w jednej linii:

```Gleam
let lowercase_text = String.to_lower("To Jest Przykładowy Tekst")
```

## Pogłębiona analiza

Gleam oferuje również możliwość ustawienia lokalizacji do konwersji tekstu. Możemy to zrobić, przekazując drugi argument do funkcji ```String.to_lower```, który jest typem ```Locale```. Domyślnie jest to ustawione na lokalizację domyślną systemu, ale możemy ją zmienić na dowolną inną, np. dla języka polskiego:

```Gleam
let text = "To Jest Przykładowy Tekst"
let locale = Locale.Make("pl_PL")
let lowercase_text = String.to_lower(text, locale)
```

Możemy również użyć funkcji ```String.to_lower_case``` jeśli wiemy, że tekst jest już w formacie Unicode i chcemy zachować przełączenia między dużymi i małymi literami, np. "Iść" zamieni się na "iść", a nie na "iśĆ". Szczegółowe informacje na temat funkcji można znaleźć w dokumentacji języka Gleam.

## Zobacz również

- Dokumentacja języka Gleam: https://gleam.run/documentation 
- Przykładowy kod na Githubie: https://github.com/gleam-lang/gleam/blob/main/standard-lib/String.md#to_lower 
- Inne metody pracy z napisami w języku Gleam: https://gleam.run/documentation/std-lib-string