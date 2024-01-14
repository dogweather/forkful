---
title:                "Elixir: Ein Datum in einen String umwandeln"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datumsangaben in Strings ist eine häufige Aufgabe beim Programmieren und kann für verschiedene Zwecke verwendet werden, z.B. für die Darstellung von Datumswerten in Benutzeroberflächen oder für die Speicherung von Datumswerten in Datenbanken. In diesem Blogbeitrag werden wir uns ansehen, wie man mit Elixir einfach Datumsangaben in Strings umwandeln kann.

## Wie geht man vor

Um eine Datumsangabe in einen String umzuwandeln, gibt es einige Funktionen in Elixir, die wir verwenden können.

### Mit der Funktion `to_string/2`

Die einfachste Möglichkeit besteht darin, die Funktion `to_string/2` zu verwenden, die uns erlaubt, ein Datum zusammen mit einem Formatierungsmuster zu übergeben und den entsprechenden String zurückzuerhalten.

Beispiel:

```Elixir
to_string(~D[2021-06-15], "{YYYY}-{MM}-{DD}")
# Ausgabe: "2021-06-15"
```

In diesem Beispiel haben wir ein Datum im Format `~D[YYYY-MM-DD]` übergeben und das Formatierungsmuster `"{YYYY}-{MM}-{DD}"`, das uns den String `2021-06-15` zurückgibt.

### Mit der Funktion `Calendar.ISO.format/2`

Eine andere Möglichkeit besteht darin, die Funktion `Calendar.ISO.format/2` zu verwenden, die eine breitere Palette von Formatierungsmustern unterstützt und die Konvertierung auch für andere Kalenderformate ermöglicht.

Beispiel:

```Elixir
Calendar.ISO.format(~D[2021-06-15], "{YYYY}-{M}-{D}")
# Ausgabe: "2021-6-15"
```

Im Vergleich zur vorherigen Funktion erhalten wir in diesem Fall eine etwas andere Ausgabe, da wir das Formatierungsmuster `"{YYYY}-{M}-{D}"` verwendet haben, das die Monatsangabe als einfache Zahl (6 statt 06) darstellt.

### Weitere Optionen

Es gibt noch weitere Funktionen und Optionen in Elixir, um Datumsangaben in Strings zu konvertieren, wie z.B. die Funktion `Timex.format/2` aus dem Timex Package oder die Module `Date` und `DateTime` aus der Elixir Standardbibliothek.

## Tiefere Einblicke

Bei der Konvertierung von Datumsangaben in Strings gibt es verschiedene Faktoren zu beachten, wie z.B. die unterschiedlichen Dateiformate (ISO, Amerikanisches Format, etc.) oder die Berücksichtigung von Zeit- und Zeitzone-Informationen.

Wenn du mehr über die Konvertierung von Datumsangaben in Strings mit Elixir erfahren möchtest, empfehlen wir dir, dich mit den verschiedenen Funktionen und Optionen vertraut zu machen und dich auch mit den verschiedenen Kalenderformaten auseinanderzusetzen.

## Siehe auch

- [Elixir Date and Time](https://elixirschool.com/de/lessons/advanced/datetime/)
- [Elixir Timex Package](https://hexdocs.pm/timex/Timex.html)
- [Elixir Standardbibliothek - Date und DateTime Module](https://hexdocs.pm/elixir/Kernel.Date.html)