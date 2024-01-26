---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:14:30.620548-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Abrufen des aktuellen Datums ist eine Möglichkeit für Programme, sich zeitlich zu orientieren. Programmierer nutzen dies für alles Mögliche: Von Log-Einträgen bis zu zeitabhängigen Funktionen.

# Wie geht das:
In Gleam geht's so:

```gleam
import gleam/utc_now.{utc_now}
import gleam/io

pub fn main() {
  let heute = utc_now()
  io.println(heute)
}
```

Wenn du das laufen lässt, siehst du etwa so etwas:

```
{{2023, 4, 10}, {12, 34, 56}}
```

# Tiefergehend
Fangen wir historisch an. In vielen älteren Sprachen gab es komplizierte Wege, um an das aktuelle Datum zu kommen. Heute hat jedes respektable Sprachsystem eine eingebaute Funktion dafür – so auch Gleam.

Alternativen? Andere Sprachen und Bibliotheken bieten manchmal mehr Customization oder komplexe Zeitberechnungen. In Gleam hält man es einfach und konkret.

Die `utc_now` Funktion gibt dir UTC-Zeit. Für lokale Zeiten musst du weitere Bibliotheken nutzen oder eigene Berechnungen anstellen.

# Siehe auch
- Gleam's Standard Library Docs: [https://hexdocs.pm/gleam_stdlib/](https://hexdocs.pm/gleam_stdlib/)
- Erlang's Calendar Module für mehr Tiefgang: [https://erlang.org/doc/man/calendar.html](https://erlang.org/doc/man/calendar.html)
- Zeitzone-Berechnungen verstehen: [https://www.iana.org/time-zones](https://www.iana.org/time-zones)
