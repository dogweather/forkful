---
title:                "Das Versenden einer http-Anfrage"
html_title:           "Javascript: Das Versenden einer http-Anfrage"
simple_title:         "Das Versenden einer http-Anfrage"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit dem Senden von HTTP-Anfragen in der aktuellen Version von JavaScript beschäftigen? Ganz einfach: die Kommunikation mit einem Server ist für moderne Webanwendungen unerlässlich. Ob Daten abrufen, Informationen senden oder Ressourcen laden - alles läuft über HTTP-Anfragen.

## Wie

Um eine HTTP-Anfrage in JavaScript zu senden, muss man zuerst eine Instanz des XMLHttpRequest-Objekts erstellen. Dieses Objekt ermöglicht die asynchrone Kommunikation mit dem Server. Dann muss man die Datenform der Anfrage angeben, z.B. JSON oder XML, und die URL des Ziels angeben. Hier ist ein Beispielcode:

```Javascript
let request = new XMLHttpRequest();
request.open('GET', 'https://www.example.com/data');
request.send();
```

Wenn die Anfrage erfolgreich ist, erhält man eine Antwort mit dem gewünschten Inhalt. Dieser kann dann weiterverarbeitet werden, z.B. durch das Manipulieren von DOM-Elementen. Man kann auch mehrere Parameter angeben, um die Anfrage zu personalisieren. 

## Deep Dive

Das XMLHttpRequest-Objekt hat verschiedene Eigenschaften, die man ändern kann, um die Kommunikation anzupassen. Mit der `onreadystatechange` Eigenschaft kann man z.B. eine Funktion festlegen, die bei Änderungen des Anfrage-Status aufgerufen wird. Dies kann nützlich sein, um Feedback während der Anfrage zu erhalten. Außerdem kann man mit `setRequestHeader()` zusätzliche Header-Informationen zur Anfrage hinzufügen.

Es gibt auch alternative Methoden für HTTP-Anfragen in JavaScript, wie z.B. die `fetch()` API. Diese ermöglicht eine noch einfachere und flexiblere Art, Anfragen zu senden. Es ist daher empfehlenswert, sich über die verschiedenen Möglichkeiten zu informieren und die für den individuellen Anwendungsfall am besten geeignete Methode auszuwählen.

## Siehe auch

- [XMLHttpRequest Objekt](https://developer.mozilla.org/de/docs/Web/API/XMLHttpRequest)
- [fetch() API](https://developer.mozilla.org/de/docs/Web/API/Fetch_API)
- [Einführung in AJAX](https://www.w3schools.com/js/js_ajax_intro.asp)