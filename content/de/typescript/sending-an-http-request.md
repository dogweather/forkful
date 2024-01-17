---
title:                "Versenden einer http-Anfrage"
html_title:           "TypeScript: Versenden einer http-Anfrage"
simple_title:         "Versenden einer http-Anfrage"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen sind ein wesentlicher Bestandteil jeder Webanwendung. Sie ermöglichen es uns, Daten von einer entfernten Quelle abzurufen und sie in unsere Anwendung zu integrieren. Programmierer verwenden HTTP-Anfragen, um ihre Anwendung mit externen APIs zu verbinden, Benutzereingaben an den Server zu senden oder einfach nur um Daten von einer anderen Webseite zu erhalten.

## Wie geht's?
Eine HTTP-Anfrage wird normalerweise mit der `fetch` -Funktion in TypeScript gesendet. Hier ist ein Beispiel dafür, wie man eine GET-Anfrage an die Google-API sendet und das Ergebnis auf der Konsole ausgibt:

```
TypeScript
fetch("https://www.googleapis.com/books/v1/volumes?q=Harry+Potter")
  .then(response => response.json())
  .then(data => console.log(data));
```

Die Ausgabe sollte eine Liste von Büchern mit dem Titel "Harry Potter" enthalten, die von der Google Books API abgerufen wurden.

Für POST-Anfragen verwenden wir die `fetch` -Funktion etwas anders, indem wir ein zweites Argument mit den erforderlichen Parameter angeben:

```
TypeScript
fetch("https://www.example.com/post-endpoint", {
  method: "POST",
  body: JSON.stringify({ username: "John", password: "password123" })
})
  .then(response => response.json())
  .then(data => console.log(data));
```

Diese Anfrage sendet Benutzername und Passwort in JSON-Format an einen POST-Endpunkt auf der angegebenen URL.

## Tiefentauchen
HTTP-Anfragen wurden Anfang der 90er Jahre entwickelt, um die Kommunikation zwischen Client und Server zu standardisieren. Heutzutage gibt es jedoch viele Alternativen wie GraphQL oder gRPC, die flexibler und effizienter sein können, je nach Anwendungsfall.

Die `fetch` -Funktion basiert auf Promises und kann daher asynchron und verketten verwendet werden. Sie können auch andere HTTP-Methoden verwenden, wie z.B. `PUT` oder `DELETE`.

Eine wichtige Sache, die bei HTTP-Anfragen zu beachten ist, ist die Sicherheit. Für Anfragen an externe APIs benötigen wir normalerweise eine API-Schlüssel, der zusammen mit dem Endpunkt angegeben werden muss. Außerdem können wir bestimmte Header hinzufügen, um die Anfrage zu authentifizieren oder die Art der Daten anzugeben, die wir senden oder empfangen möchten.

## Siehe auch
Weitere Informationen zur `fetch` -Funktion und zum Senden von HTTP-Anfragen finden Sie in der offiziellen [Dokumentation](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API).

Um mehr über alternative Protokolle wie GraphQL zu erfahren, schau dir diese [Einführung](https://graphql.org/learn/) an.

Für Einblicke in die Implementierung von HTTP-Anfragen in TypeScript gibt es [hier](https://github.com/microsoft/TypeScript/blob/master/lib/lib.dom.d.ts#L3323) eine Übersicht über die `fetch` -Funktion aus dem offiziellen TypeScript-Repository.