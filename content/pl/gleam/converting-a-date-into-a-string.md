---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Gleam: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwertowanie daty na ciąg znaków jest jednym z najczęściej wykonywanych zadań przez programistów. Polega to na przekształceniu daty zapisanej w formacie liczbowym do formy tekstowej, idealnie dostosowanej do potrzeb programu. Dlaczego? Przede wszystkim po to, by móc łatwo manipulować datami i wyświetlać je w czytelnej formie dla użytkownika.

## Jak to zrobić?

Możemy wykonać to zadanie w Gleam za pomocą wbudowanego modułu `Calendar`. Najpierw musimy zaimportować ten moduł, a następnie używać funkcji z niego, aby skonwertować datę na ciąg znaków.

```Gleam
import Calendar

let date = Calendar.local_time(2021, 02, 03)

// Konwersja do ciągu znaków w formacie ISO-8601
let str = Calendar.to_rfc3339(date)
// "2021-02-03T00:00:00+00:00"
```

Możemy również użyć innych funkcji z modułu `Calendar`, aby dostosować format wyjściowego ciągu znaków, np. `to_local_date_string()` lub `to_local_time_string()`. Pełną listę funkcji można znaleźć w dokumentacji Gleam.

## Głębsza analiza

Historia konwersji daty na ciąg znaków sięga czasów, gdy programy nie mogły bezpośrednio obsługiwać operacji na datach. Wówczas konieczne było przekształcenie daty do postaci tekstowej, aby móc z nią pracować. Współczesne języki programowania, takie jak Gleam, mają już wbudowane funkcje konwertujące daty, co znacznie ułatwia pracę programistów.

Alternatywą dla korzystania z wbudowanych funkcji jest użycie bibliotek zewnętrznych, np. `Dateformat` lub `Datetime`. Jednakże, w przypadku Gleam, zaleca się korzystanie z modułu `Calendar`, ponieważ jest to wbudowane rozwiązanie, które jest zgodne z uniwersalnym standardem ISO-8601.

W praktyce, konwersja daty do ciągu znaków wymaga przeprowadzenia kilku operacji, takich jak przekształcenie liczbowych wartości daty w odpowiednie oznaczenia, np. miesiące zamiast liczb, czy dodanie odpowiednich separatorów. Dzięki użyciu wbudowanych funkcji, unikamy błędów i oszczędzamy czas.

## Zobacz również

Dokumentacja Gleam dotycząca modułu `Calendar`: https://gleam.run/modules/gleam/calendar/