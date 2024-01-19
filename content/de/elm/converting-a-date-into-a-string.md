---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Konvertieren eines Datums in eine Zeichenkette (auch als String bekannt) ist ein weit verbreiteter Prozess in der Programmierung, bei dem wir einen Datums-/Zeitstempel in einen lesbaren Text konvertieren. Dies erleichtert häufig die Darstellung und Analyse der Daten.

## So geht's:

Elm bietet die `Date` und `Time` Module für das Handling von Datum und Zeit. Hier ist ein einfaches Beispiel, wie man ein Datum in einen String konvertiert.

```Elm
import Time
import Time.Extra

main =
    let
        time =
            Time.millisToPosix 1552063680000
    in
    Time.Extra.format "dd.MM.yyyy" time
```

In diesem Beispiel wird die `format` Funktion aus dem `Time.Extra` Modul verwendet, um ein Datum in einem bestimmten Format darzustellen (hier "dd.MM.yyyy"). Der obige Code liefert "08.03.2019" als Ausgabe.

## Tief Eintauchen

### Historischer Kontext
Die Date-to-String-Konvertierung ist schon sehr lange ein grundlegendes Feature in fast allen Programmiersprachen. Der Bedarf hierfür entstand durch die Notwendigkeit, Datumsangaben menschenlesbar zu machen, sei es für die Anzeige in der Benutzeroberfläche oder für Berechnungen und Vergleiche.

### Alternativen
In Elm könnten wir auch die 'toString' Funktion verwenden, um ein Datum in einen String umzuwandeln, aber dann haben wir wenig Kontrolle über das Format des resultierenden Strings.

```Elm
import Time

main =
    let
        time =
            Time.millisToPosix 1552063680000
    in
    String.fromInt time
```

Dies gibt die genaue Zeit in Millisekunden seit dem Unix-Epoch-Zeitpunkt (01. Januar 1970) zurück.

### Implementierungsdetails
Das `Time.Extra` Modul in Elm verwendet hinter den Kulissen JavaScript's eingebautes `Date` und `Time` Objekte, um seine Funktionen zu ermöglichen. Es bietet aber auch zusätzliche Funktionen, wie das Formatieren von Datums- und Zeitstrings.

## Siehe auch

Für weitere Informationen und Beispiele zur Programmierung in Elm, schauen Sie bitte in die offizielle [Elm Dokumentation](https://elm-lang.org/docs) und auch im [Elm Time Package](https://package.elm-lang.org/packages/elm/time/latest). 

Für tiefergehendes Verständnis der Datums- und Zeitbearbeitung in Elm kann das [Elm Time Extra-Paket](https://package.elm-lang.org/packages/justinmimbs/date/latest/) nützlich sein. Es bietet zusätzliche Funktionen wie Zeitprüfungen und -berechnungen.