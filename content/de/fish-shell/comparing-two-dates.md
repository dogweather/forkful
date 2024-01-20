---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Datumvergleiche prüfen, ob ein Datum vor oder nach einem anderen liegt. Programmierer verwenden diese Funktion zum Sortieren, Filtern oder Schaffen von Zeitleisten in ihren Applikationen.

## Wie geht das?

Angenommen, wir haben zwei Daten, die wir vergleichen möchten. Lass uns das tun! Hier ist, wie man es in der Fish Shell macht:

```Fish Shell
set datum1 (date -u -d "2020-03-20" +%s)
set datum2 (date -u -d "2021-03-20" +%s)

if test $datum1 -gt $datum2
    echo "Datum1 ist später als Datum2."
else if test $datum1 -eq $datum2
    echo "Datum1 ist das gleiche wie Datum2."
else 
    echo "Datum1 ist früher als Datum2."
end
```
Die Ausgabe, die wir erhalten, lautet:

```
Datum1 ist früher als Datum2.
```

## Vertiefung

Im historischen Kontext wurde der Vergleich zweier Daten stark von der Implementierung der Unix-Zeit beeinflusst, die die Sekunden seit dem 1. Januar 1970 zählt. 
Alternativen für die Konvertierung und den Vergleich von Daten könnten in anderen Schalen oder Programmiersprachen, wie z.B. Python oder JavaScript, gefunden werden. Die oben gezeigte Implementierung in der Fish Shell verwendet die Unix timestamp Methode, weil es die genaueste Art ist, zwei Daten zu vergleichen.

## Weiterführende Links

- [UNIX timestamp – Wikipedia](https://de.wikipedia.org/wiki/Unixzeit)
- [Fish - eine smarte Shell für den Terminal | heise Download](https://www.heise.de/download/product/fish-50937)