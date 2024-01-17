---
title:                "Zwei Daten vergleichen"
html_title:           "Bash: Zwei Daten vergleichen"
simple_title:         "Zwei Daten vergleichen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen von zwei Daten ist ein gängiger Programmieraufgabe, bei der man untersucht, ob ein Datum früher, später oder gleich ist wie ein anderes Datum. Programmierer nutzen dies, um beispielsweise zu überprüfen, ob ein bestimmtes Datum bereits vergangen ist oder in der Zukunft liegt.

## Anleitung:
Hier sind zwei Beispiele, wie man mit Bash zwei Daten vergleichen kann:

```Bash
date_1="2021-10-31"
date_2="2021-11-01"

if [[ "$date_1" -gt "$date_2" ]]; then
  echo "$date_1 ist später als $date_2"
elif [[ "$date_1" -lt "$date_2" ]]; then
  echo "$date_1 ist früher als $date_2"
else
  echo "Beide Daten sind gleich"
fi
```

```Bash
date_1="2021-10-31"
date_2="2021-11-01"

if [[ "$date_1" > "$date_2" ]]; then
  echo "$date_1 ist später als $date_2"
elif [[ "$date_1" < "$date_2" ]]; then
  echo "$date_1 ist früher als $date_2"
else
  echo "Beide Daten sind gleich"
fi
```

Ausgabe:
```Bash
2021-10-31 ist früher als 2021-11-01
```

## Tiefer Eintauchen:
Das Vergleichen von Daten hat eine lange Geschichte und wurde bereits in der Anfangszeit der Programmierung verwendet. Es gibt verschiedene Methoden, um zwei Daten zu vergleichen, wie z.B. den Vergleich von Unix-Zeitstempeln oder das Konvertieren von Daten in ein dezimales Format und direkten Vergleich. Die Implementierung hängt auch von der verwendeten Programmiersprache ab.

## Siehe auch:
- [Ein W3Schools-Artikel über das Vergleichen von Daten in Bash](https://www.w3schools.com/whatis/whatis_compare.asp)
- [Ein Wikipedia-Artikel über Datums- und Zeitrechnung](https://de.wikipedia.org/wiki/Datums-_und_Zeitrechnung)
- [Ein Beitrag über die Geschichtedes Unix-Zeitstempels](https://www.e-tutorial.info/unix-time-and-timezone/)