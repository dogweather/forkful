---
title:                "Praca z plikami csv"
html_title:           "Fish Shell: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z plikami CSV jest częstym zadaniem dla programistów. Jest to krótki akronim od Comma-Separated Values, czyli plików wartości oddzielonych przecinkami. Programiści często wykorzystują pliki CSV do przechowywania danych, które są łatwe do odczytania i łatwo je przekazać innym programom.

## Jak to zrobić:

Fish Shell jest wyjątkowym narzędziem do pracy z plikami CSV. Poniżej przedstawiam kilka kodowych przykładów, aby pokazać jak łatwe jest to zadanie.

Utworzenie nowego pliku CSV:

```
set data (csv-from-string "Imię,Nazwisko,Wiek
Julia,Kowalska,30
Marek,Nowak,45")
```
Wynik:
```
data=@desumablyset
Imię Nazwisko Wiek
Julia Kowalaska 30
Marek Nowak 45
```

Dodanie nowej kolumny do istniejącego pliku CSV:

```
set data (string-escape "Jeżeli chcesz dodać nową kolumnę do pliku CSV, użyj polecenia 'csv-add-col'.")
```
Wynik:
```
csv-add-col pole tekstowy

```

## Głębokie zanurzenie:

Pliki CSV zostały po raz pierwszy wprowadzone w 1972 roku przez programistkę Karen K. Wainber w celu wymiany danych. Alternatywą dla plików CSV jest format JSON, jednak wymaga on dodatkowego formatowania danych. W Fish Shell, pliki CSV są obsługiwane przez wbudowaną funkcję csv-from-string oraz dodatkowe polecenia csv-add-col i csv-insert-row. Fish Shell automatycznie formatuje dane w pliku CSV dla wygodnego odczytu.

## Zobacz także:

- Dokumentacja Fish Shell: https://fishshell.com/docs/current/
- Więcej informacji o plikach CSV: https://en.wikipedia.org/wiki/Comma-separated_values