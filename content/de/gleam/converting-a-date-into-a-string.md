---
title:                "Umwandeln eines Datums in einen String"
html_title:           "Gleam: Umwandeln eines Datums in einen String"
simple_title:         "Umwandeln eines Datums in einen String"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Konvertieren eines Datums in einen String ist ein häufiges Problem für Programmierer, bei dem ein bestimmtes Datum in einen lesbaren Formatstring umgewandelt wird. Dies kann hilfreich sein, um das Datum in einem Textformat anzuzeigen oder es in einer Datenbank zu speichern. 

## Wie geht's?

Das Gleam `Time`-Modul bietet die `Format.date`-Funktion, um ein Datum in einen Formatstring umzuwandeln. Es wird das Datum und das gewünschte Format als Argumente angenommen und gibt den entsprechenden String zurück.

```Gleam
import Time

let date = Time.now()

let date_string = Time.Format.date(date, "%Y-%m-%d")
```

Die Ausgabe des oben genannten Beispiels wäre `2021-07-15`.

## Tiefere Einblicke

Das Konvertieren von Datumsangaben in einen String ist ein häufiges Problem, das Entwickler seit langem lösen müssen. Es gibt verschiedene Ansätze, um dies zu erreichen, wie zum Beispiel das Schreiben eigener Funktionen oder das Verwenden von Bibliotheken von Drittanbietern. Die Gleam `Time`-Bibliothek bietet jedoch eine einfache und effektive Lösung, die direkt in die Gleam-Sprache integriert ist.

## Siehe auch

Offizielle Dokumentation des `Time`-Moduls: https://gleam.run/modules/time/

Weitere Informationen zum Formatieren von Zeitangaben: https://strftime.org/