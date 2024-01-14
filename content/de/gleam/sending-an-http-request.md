---
title:                "Gleam: Senden einer HTTP-Anfrage"
simple_title:         "Senden einer HTTP-Anfrage"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen ist ein wichtiger Bestandteil der modernen Webentwicklung. Ob Sie Daten von einer API abrufen, Benutzeraktionen verarbeiten oder einfach nur eine Website besuchen - alles basiert auf dem Austausch von HTTP-Anfragen und -Antworten. Daher ist es wichtig, zu verstehen, wie man das Senden von HTTP-Anfragen in der Programmiersprache Gleam umsetzen kann.

## Wie geht das?

Der erste Schritt zum Senden einer HTTP-Anfrage in Gleam ist die Verwendung des Moduls `Httpc`. Hier ist ein Beispielcode, der eine einfache GET-Anfrage an die URL "https://www.example.com" sendet und die Antwort als String zurückgibt:

```Gleam
import gleam/httpc

let response = httpc.get("https://www.example.com")
```

Die Variable `response` enthält nun das Ergebnis der Anfrage, das in diesem Fall ein String ist. Um die Antwort zu verarbeiten, können Sie die integrierten Funktionen von Gleam verwenden oder eine externe Bibliothek wie `json` für die Verarbeitung von JSON-Daten einbinden.

Wenn Sie eine POST-Anfrage senden möchten und Daten an den Server senden müssen, können Sie dies in der `httpc.post`-Funktion angeben:

```Gleam
let response = httpc.post("https://www.example.com", `
  {"name": "Max Mustermann", "age": 30}
`)
```

In diesem Beispiel werden die Daten als JSON-String an den Server gesendet. Sie können jedoch auch andere Datenformate wie XML oder Formulardaten senden, indem Sie die `httpc.body`-Funktion verwenden.

## Tiefen-Tauchgang

Die `httpc`-Funktionen sind sehr einfach zu verwenden und können grundlegende Anforderungen für die meisten Anwendungsfälle erfüllen. Wenn Sie jedoch eine genauere Kontrolle über die Anfrage und die Antwort benötigen, können Sie die `httpc.request`-Funktion verwenden.

Diese Funktion erwartet ein `Request`-Record als Eingabe, das detaillierte Informationen über die Anfrage enthält, wie z.B. die HTTP-Methode, die URL, Header und den Anfragekörper. Sie können auch eine Konfiguration angeben, um die Verwendung von HTTP-Authentifizierung oder SSL zu aktivieren.

## Weiterführende Links

- Dokumentation für das `Httpc`-Modul: https://gleam.run/modules/gleam/httpc/
- Externe `json`-Bibliothek: https://github.com/gleam-lang/gleam/blob/master/lib/json/
- Weitere Gleam-Module für die Webentwicklung: https://gleam.run/modules/