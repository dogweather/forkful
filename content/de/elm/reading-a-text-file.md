---
title:                "Elm: Das Lesen einer Textdatei"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Textdateien sind ein häufig verwendetes Dateiformat in der Programmierung und können hilfreich sein, um Daten zu speichern oder zu übertragen. In diesem Blogbeitrag werden wir uns damit beschäftigen, wie man Textdateien in Elm lesen kann.

# Wie geht man vor?

Um eine Textdatei in Elm zu lesen, muss man zunächst die `File`-Library importieren. Dann kann man die Funktion `readFile`, die als Argument den Dateinamen erwartet, verwenden, um den Inhalt der Datei zu lesen. Im folgenden Beispiel lesen wir eine Datei namens "beispiel.txt" und geben den Inhalt über die Konsole aus.

```Elm
import File

File.readFile "beispiel.txt"
    |> Task.attempt handleResult
    
handleResult : Result File.Error String -> Cmd msg
handleResult result =
    case result of
        Ok content ->
            (Debug.log "Dateiinhalt:" content)
    
        Err error ->
            (Debug.log "Fehler beim Lesen der Datei:" error)
```

Dieses Beispiel verwendet das `Debug`-Modul, um den Inhalt der Datei in der Konsole auszugeben. In der `handleResult`-Funktion wird zuerst überprüft, ob das Lesen der Datei erfolgreich war. Wenn ja, wird der Inhalt ausgegeben, ansonsten wird ein Fehler gemeldet.

# Tiefgehende Informationen

Das `File`-Modul bietet auch andere Funktionen, um mit Textdateien zu arbeiten, wie zum Beispiel `writeFile` zum Schreiben von Daten in eine Datei. Außerdem gibt es die Möglichkeit, mit Hilfe von Decodern und Encodern die Daten aus der Datei in anderer Form zu lesen oder zu schreiben.

Es ist auch wichtig zu beachten, dass das Bearbeiten von Dateien in Elm nicht direkt auf dem lokalen Computer des Benutzers erfolgt, sondern über die Web-APIs. Daher müssen die Dateien über einen Server bereitgestellt werden oder es muss die Erlaubnis des Benutzers eingeholt werden, um auf die Dateien auf seinem Gerät zuzugreifen.

# Siehe auch

- [Offizielle Dokumentation zum File-Modul in Elm](https://package.elm-lang.org/packages/elm/file/latest/)
- [Ein Einführungsartikel zu Elm](https://medium.com/swlh/an-introduction-to-elm-3c5b4275addc)
- [Der offizielle Guide zu Elm](https://guide.elm-lang.org/)