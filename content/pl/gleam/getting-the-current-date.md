---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie aktualnej daty to prosty kod, który ściąga i wyświetla bieżący dzień, miesiąc i rok. Programiści używają tego, aby podążać za rzeczywistym czasem, monitorować zdarzenia i tworzyć dzienniki.

## Jak to zrobić:

Poniżej znajduje się proste użycie modułu `datetime` w języku Gleam.

```Gleam
import gleam/datetime.{datetime, now, utc}

fn get_current_date() {
  let dt = now()
  datetime(dt, utc)
}
```

Po uruchomieniu powyższego kodu, kod wydrukowałby coś w stylu `2023-02-20T10:15:30Z`. 

## Dogłębnie:

W kontekście historycznym, we wczesnej informatyce, pobieranie aktualnej daty było trudnym zadaniem, wymagającym skomplikowanych obliczeń. Ale dzięki modernizacji i rozwojowi bibliotek programistycznych, takich jak moduł `datetime` w języku Gleam, stało się to o wiele łatwiejsze.

Alternatywą dla użycia funkcji `now()` jest przekazanie konkretnej daty i czasu do funkcji `datetime()`. Możesz to wykorzystać, gdy chcesz utworzyć specyficzny moment w czasie, zamiast używać aktualnego.
 
Ciekawą rzeczą w implementacji `datetime` jest to, że jest on oparty na UTC, czyli Coordinated Universal Time. To oznacza, że niezależnie od strefy czasowej, zawsze otrzymasz czas UTC.

## Zobacz także:

Jeśli chcesz dowiedzieć się więcej o modułach czasu i dacie w Gleam, zobacz rowniez:
2. GitHub Repo Gleam: [Link](https://github.com/gleam-lang/gleam)