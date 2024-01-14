---
title:                "Elixir: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datum in einen String ist eine häufige Aufgabe in der Elixir-Programmierung. Es ermöglicht uns, Daten in einem leicht lesbaren Format auszugeben und macht es einfacher, sie mit anderen zu teilen. Erfahre in diesem Blog-Post, wie du ein Datum in einen String umwandeln kannst.

## Wie geht's?

Lass uns zunächst einen Blick auf das `Date`-Modul in Elixir werfen. Mit diesem Modul haben wir Zugriff auf eine Reihe von Funktionen, die uns helfen, mit Datumswerten zu arbeiten. Eine dieser Funktionen ist `to_string/1`, die ein Datum in einem String im Format YYYY-MM-DD zurückgibt.

```Elixir
Date.to_string(~D[2020-12-01])
```
Ausgabe: "2020-12-01"

Wir können auch eine benutzerdefinierte Formatierung für unseren String angeben, indem wir die Funktion `to_string/2` verwenden und ein Formatierungsmuster angeben.

```Elixir
Date.to_string(~D[2020-12-01], "{YYYY}/{MM}/{DD}")
```
Ausgabe: "2020/12/01"

Es ist auch möglich, ein `DateTime`-Objekt in einen String umzuwandeln. Hierfür verwenden wir die Funktion `to_string/1` im `DateTime`-Modul und geben ein Dateiformat als Parameter an.

```Elixir
DateTime.to_string(~U[2020-12-01 12:30:00Z], "{YYYY}-{M}-{D}T{H}:{M}")
```
Ausgabe: "2020-12-01T12:30"

## Deep Dive

Die Funktion `to_string/2` im `Date`-Modul akzeptiert auch einen optionalen Zeitzonenparameter. Standardmäßig wird die lokale Systemzeit verwendet, aber wir können auch eine bestimmte Zeitzone angeben, indem wir ein `DateTime`-Objekt als Parameter übergeben.

```Elixir
DateTime.to_string(~U[2020-12-01 12:00:00], "{YYYY}-{MM}-{DD}T{H}:{M}", "America/New_York")
```
Ausgabe: "2020-12-01T12:00"

Wenn wir uns das Ergebnis genau ansehen, werden wir feststellen, dass die Zeit um eine Stunde versetzt ist. Das liegt daran, dass die Zeitzone "America/New_York" eine Stunde hinter der Systemzeit liegt.

Es ist auch möglich, eine Zeitzone als Atom anzugeben. Dies ist hilfreich, wenn wir die Zeitzone in einer Konfigurationsdatei speichern und sie dann in unseren Code einbinden möchten.

## Siehe auch

[Date-Modul Dokumentation](https://hexdocs.pm/elixir/Date.html)

[DateTime-Modul Dokumentation](https://hexdocs.pm/elixir/DateTime.html)

[Zeitzonen in Elixir](https://dev.to/adelcambre/getting-to-know-time-zones-in-elixir-3ed1)