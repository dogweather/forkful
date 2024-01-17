---
title:                "Vergleichen von zwei Datum"
html_title:           "Elm: Vergleichen von zwei Datum"
simple_title:         "Vergleichen von zwei Datum"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Was & Warum?
Vergleichen von zwei bestimmten Datums ist ein häufiger Vorgang in der Programmierung. Es ermöglicht Programmierern, zu prüfen, ob zwei Datumsangaben gleich sind oder welches früher oder später liegt.

# Wie?
Im Folgenden finden Sie zwei Beispiele, wie Sie in Elm zwei Datumsangaben miteinander vergleichen können:

- Mit dem ```Date.compare``` Befehl können Sie zwei Datumsangaben vergleichen und erhalten entweder ```LT``` (lower than), ```EQ``` (equal) oder ```GT``` (greater than) als Ergebnis.

```
Elm.Date.compare (Elm.Date.fromTime 1594624800) (Elm.Date.fromTime 1594871431)
-- GT
```

- Ein weiterer Weg ist die Verwendung der Funktion ```compareDates```, die in der Elm-Standardbibliothek definiert ist. Diese Funktion gibt eine numerische Antwort zurück, wobei ```1``` für ```LT```, ```2``` für ```EQ``` und ```3``` für ```GT``` steht.

```
import Date exposing (..)

compareDates
    (fromTime 1594624800)
    (fromTime 1594871431)
-- 3
```

# Tiefere Einblicke
Das Vergleichen von Daten ist ein wichtiger Teil der historischen Entwicklung der Programmiersprachen. Frühere Versionen von Sprachen wie C oder Java verfügten zunächst nicht über integrierte Funktionen oder Bibliotheken für Datumsvorgänge. Stattdessen mussten Entwickler ihre eigenen Methoden schreiben oder auf externe Bibliotheken zurückgreifen.

Alternativen zur Vergleichung von Datein in Elm sind beispielsweise die Verwendung von Vergleichsoperatoren wie ```<``` und ```>```, die jedoch nur bei numerischen Datumsangaben anwendbar sind.

Die Details der Implementierung von Datumsvorgängen in Elm sind in der offiziellen Dokumentation zu finden, einschließlich der Unterteilung von Datumswerten in Jahr, Monat, Tag usw.

# Siehe auch
Weitere Informationen und Beispiele zu Datumsvorgängen in Elm in der offiziellen Dokumentation unter https://elm-lang.org/.