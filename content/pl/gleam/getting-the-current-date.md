---
title:                "Pobieranie aktualnej daty"
html_title:           "Gleam: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Aktualna wersja języka programowania Gleam zapewnia wiele przydatnych funkcji, w tym możliwość uzyskania aktualnej daty. Dzięki temu, możemy wykorzystać tę datę w naszym kodzie, np. do tworzenia dynamicznych aplikacji lub generowania raportów. 

## Jak to zrobić

Aby uzyskać aktualną datę w Gleam, możemy skorzystać z wbudowanej biblioteki ```built_in```, która dostarcza nam moduł ```Time```. W nim znajduje się funkcja ```now/0```, która zwraca aktualną datę i godzinę w formacie ```Time.DateTime```. 

Przykładowy kod: 
```Gleam 
import built_in/Time

let current_date = Time.now()

// Output: 2021-03-17T23:27:07 
```

Możemy również wybrać wygodniejszy dla nas format, korzystając z funkcji ```format/?``` z modułu ```Time.Format```. Przykładowo, jeśli chcemy uzyskać tylko datę w formacie YYYY-MM-DD, możemy użyć takiego kodu: 

```Gleam 
import built_in/Time
import built_in/Time.Format

let current_date = Time.now() |> Time.Format.format("YYYY-MM-DD")

// Output: 2021-03-17 
```

## Głębszy wgląd

Funkcja ```now/0``` zwraca obiekt ```Time.DateTime```, który składa się z różnych pól, takich jak rok, miesiąc, dzień, godzina, minuta, sekunda itp. Możemy więc wykorzystać te pola, aby uzyskać różne wartości, np. rok lub miesiąc, tylko gdy są nam potrzebne. 

```Gleam 
import built_in/Time
import built_in/Time.Format

let current_date = Time.now()

let year = current_date.year
let month = current_date.month

// Output:
year: 2021
month: 3 
```

Możemy również użyć funkcji ```date/0```, aby uzyskać tylko datę bez godziny i minut. 

```Gleam 
import built_in/Time
import built_in/Time.Format

let current_date = Time.date()

// Output: 2021-03-17 
```

## Zobacz także

- Dokumentacja biblioteki ```built_in```: https://gleam.run/documentation/builtin 
- Przykład wykorzystania aktualnej daty w aplikacji Gleam: https://github.com/lpil/monome.norns/tree/master/libs/current_time