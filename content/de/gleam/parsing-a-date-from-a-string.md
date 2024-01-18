---
title:                "Einen Datumswert aus einer Zeichenfolge analysieren"
html_title:           "Gleam: Einen Datumswert aus einer Zeichenfolge analysieren"
simple_title:         "Einen Datumswert aus einer Zeichenfolge analysieren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String ist ein gängiges Problem in der Programmierung, bei dem ein Datum, das als Text in einer bestimmten Form angegeben ist, in ein Datumobjekt umgewandelt wird, das in der Programmiersprache verwendet werden kann. Dies ist wichtig, da viele APIs oder Datenbanken Datumsangaben in Form von Strings anfordern, um Informationen auszutauschen oder zu speichern.

## Wie geht's?
Ein Beispiel für das Parsen eines Datums aus einem String in Gleam könnte wie folgt aussehen:
```Gleam
let string = "3. August 2020"
let pattern = "%d.%B %Y"
let date = Date.Parse(string, pattern)
```
Dies erzeugt ein Datumobjekt mit dem Wert 3. August 2020. Die Funktion `Parse` nimmt zwei Argumente an: den String mit dem Datum und das Muster, in dem das Datum angegeben ist. Das Muster wird anhand von Platzhaltern wie `%d` für den Tag, `%B` für den Monat und `%Y` für das Jahr definiert.

Ein weiteres Beispiel mit einer anderen Datumsangabe könnte so aussehen:
```Gleam
let string = "25/12/2020"
let pattern = "%d/%m/%Y"
let date = Date.Parse(string, pattern)
```
Dieses Mal erzeugt die Funktion ein Datumobjekt mit dem Wert 25. Dezember 2020.

## Tiefentauchen
Das Parsen von Datumsangaben stammt aus der Programmiersprache C und wird heute von vielen Sprachen, einschließlich Gleam, unterstützt. Alternativ gibt es auch Bibliotheken wie `dateutils` oder `moment.js` für erweiterte Funktionalitäten.

Die Implementierung des Parsings in Gleam basiert auf der Standardbibliothek `erlang` und verwendet Funktionen wie `datetime:parse` und `erlang:list_to_integer`.

## Sieh dir auch an
- [Einführung in Datums- und Uhrzeitmanipulation in Gleam](https://gleam.run/news/2021-01-14-dates)
- [Dokumentation zur Date-Modul in Gleam](https://gleam.run/modules/stdlib/Date.html)