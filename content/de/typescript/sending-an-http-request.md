---
title:                "TypeScript: Versenden einer http Anfrage"
simple_title:         "Versenden einer http Anfrage"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum jemand in der Entwicklungsfeld auf TypeScript setzen sollte. Einer dieser Gründe ist die Fähigkeit, HTTP-Anfragen zu versenden. Das Versenden von HTTP-Anfragen ermöglicht es einem, mit Servern oder APIs zu kommunizieren und Daten zu empfangen oder zu senden. Dies ist besonders wichtig für die Entwicklung von Webanwendungen oder mobilen Anwendungen, die auf das Internet zugreifen müssen.

## Wie geht es

Um eine HTTP-Anfrage in TypeScript zu senden, müssen wir zunächst das `http`-Modul importieren. Dann können wir die Funktion `request` verwenden, um unsere Anfrage an eine bestimmte URL zu senden. Hier ist ein Beispielcode, um eine GET-Anfrage mit TypeScript zu senden:

```TypeScript
import http from 'http';

http.request('http://example.com', (response) => {
    console.log(response.statusCode); // prints 200
});
```

Dieser Code verwendet die `request`-Funktion, um eine Anfrage an die URL "http://example.com" zu senden. Sobald die Antwort empfangen wird, wird die Funktion, die wir dem `request`-Aufruf übergeben haben, aufgerufen und die Response-Objekt wird zurückgegeben. In diesem Beispiel nutzen wir das `statusCode`-Eigenschaft, um den Statuscode der Antwort zu erhalten, der uns sagt, ob die Anfrage erfolgreich war oder nicht.

## Tiefentauchen

Es gibt viele verschiedene Optionen und Eigenschaften, die wir in TypeScript nutzen können, wenn wir eine HTTP-Anfrage senden. Wir können zum Beispiel auch Daten in der Anfrage senden, indem wir sie dem `request`-Aufruf als zweiten Parameter hinzufügen. Wir können auch bestimmte Header oder Authentifizierungs-Token in der Anfrage angeben.

Es ist auch wichtig zu beachten, dass TypeScript standardmäßig asynchrone Funktionen verwendet, wenn wir HTTP-Anfragen senden. Das bedeutet, dass wir entweder die `async/await` Syntax nutzen oder Promises verwenden müssen, um auf die Antwort zu warten und mit den Daten zu arbeiten.

In TypeScript gibt es auch viele Bibliotheken und Frameworks, die speziell für das Versenden von HTTP-Anfragen entwickelt wurden, wie zum Beispiel "axios" oder "isomorphic-fetch". Diese können oft noch leistungsfähiger und leichter zu handhaben sein als das Verwenden des nativen `http`-Moduls.

## Siehe auch

- https://nodejs.org/api/http.html - Die offizielle Dokumentation des `http`-Moduls von Node.js
- https://www.typescriptlang.org/docs/handbook/interfaces.html - Mehr über die Verwendung von TypeScript-Interfaces, um HTTP-Anfragen zu definieren
- https://github.com/axios/axios - Die "axios" Bibliothek für das Senden von HTTP-Anfragen in TypeScript oder JavaScript