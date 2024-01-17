---
title:                "Das heutige Datum abrufen"
html_title:           "Elixir: Das heutige Datum abrufen"
simple_title:         "Das heutige Datum abrufen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist ein wichtiger Teil des Programmierens in Elixir. Es ermöglicht es uns, die aktuelle Zeit und Datum in unseren Programmen zu verwenden, was für die Benutzerfreundlichkeit und Funktionalität entscheidend sein kann. Programmierer nutzen dies, um Echtzeitfunktionen in ihren Anwendungen zu implementieren oder um einfach das aktuelle Datum für ihre eigenen Zwecke abzurufen.

## Wie geht es?
Wir können das aktuelle Datum in Elixir auf verschiedene Arten abrufen. Eine Möglichkeit ist die Verwendung der Funktion `Date.utc_today`, die das aktuelle Datum im UTC-Format zurückgibt. Hier ist ein Beispiel-Code mit der Ausgabe:

```Elixir
Date.utc_today() 
#=> {:ok, ~U[2021-03-20 00:00:00Z]}
```

Wir können auch die Funktion `DateTime.utc_now` verwenden, um das aktuelle Datum und die aktuelle Uhrzeit im UTC-Format zu erhalten. Hier ist ein Beispiel-Code mit der Ausgabe:

```Elixir
DateTime.utc_now() 
#=> {:ok, ~N[2021-03-20 17:45:00Z]}
```

Schließlich können wir die Funktion `:calendar.universal_time/0` verwenden, um das aktuelle Datum und die aktuelle Zeit im lokalen Format zu erhalten. Hier ist ein Beispiel-Code mit der Ausgabe:

```Elixir
:calendar.universal_time() 
#=> {{2021, 3, 20}, {17, 45, 00}}
```

## Tiefer tauchen
Elixir verwendet unter der Haube die Erlang-Laufzeitumgebung mit dem Namen "Erlang Virtual Machine (EVM)". Dies ermöglicht es Elixir, auf das Datum über die Erlang-Bibliothek `calendar` zuzugreifen. Es gibt auch andere Möglichkeiten, das aktuelle Datum in Elixir zu erhalten, wie zum Beispiel die Verwendung von externen Bibliotheken wie `timex` oder `calendar_date`.

## Siehe auch
- [Elixir-Dokumentation zu Date und Time](https://hexdocs.pm/elixir/Date.html)
- [Erlang-Dokumentation zu `calendar`](http://erlang.org/doc/man/calendar.html)
- [Elixir-Forum-Thread zu Getting the Current Date](https://elixirforum.com/t/getting-the-current-date/2806)