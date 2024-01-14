---
title:                "Elm: Der aktuelle Datumswert erhalten"
simple_title:         "Der aktuelle Datumswert erhalten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum
Als Programmierer kann es manchmal nützlich sein, das aktuelle Datum in einem Programm abzurufen. Ob es darum geht, ein Geburtsdatum zu überprüfen oder einfach nur um die aktuelle Zeit anzuzeigen - das Abrufen des aktuellen Datums ist ein häufiger Bedarf in der Programmierung. In diesem Blog-Beitrag werden wir uns ansehen, wie man das aktuelle Datum in Elm abruft und nutzen kann.

## So geht's
Um das aktuelle Datum in Elm abzurufen, müssen wir die `Time.now` Funktion verwenden. Diese Funktion gibt uns ein `Time.Posix` Objekt zurück, das das aktuelle Datum und die Uhrzeit enthält. Hier ist ein Beispielcode, der das aktuelle Datum und die Uhrzeit in einer Variablen speichert und dann in der Konsole ausgibt:

```Elm
import Time

main =
  Time.now
    |> Time.toIsoString
    |> text
    |> Debug.log "Aktuelles Datum und Uhrzeit:"
```

Hier sehen wir, dass wir zuerst das `Time` Modul importieren müssen, um auf die `now` und `toIsoString` Funktionen zugreifen zu können. Dann verwenden wir die `now` Funktion, um das aktuelle Datum abzurufen, und `toIsoString`, um es in ein lesbares ISO-Format umzuwandeln. Wir geben das Ergebnis schließlich mit `Debug.log` in der Konsole aus.

Das obige Beispiel gibt folgenden Output in der Konsole aus:

```
Aktuelles Datum und Uhrzeit: 2021-08-17T15:06:31.653Z
```

Es ist wichtig zu beachten, dass das Datum und die Uhrzeit in der UTC-Zeitzone zurückgegeben werden. Um die lokale Zeitzone zu berücksichtigen, können wir die `Zone.now` Funktion verwenden und das Ergebnis mit `Time.inLocalZone` konvertieren.

## Tiefergehende Informationen
Für Entwickler, die mehr über die Implementierung der `Time` und `Zone` Module erfahren möchten, gibt es die Dokumentation im Elm Package Index (Elm-Paket-Index). Dort findet man detailliertere Informationen über die Funktionen und deren Rückgabewerte. Es ist auch hilfreich zu verstehen, wie `Posix` und andere Datentypen in Elm funktionieren, um das aktuelle Datum besser zu nutzen.

## Siehe auch
- [Dokumentation zur `Time` Library im Elm-Paket-Index](https://package.elm-lang.org/packages/elm/time/latest/)
- [Dokumentation zur `Zone` Library im Elm-Paket-Index](https://package.elm-lang.org/packages/elm/time/latest/Zone)
- [Dokumentation zum `Posix` Datumstyp im Elm-Paket-Index](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix)