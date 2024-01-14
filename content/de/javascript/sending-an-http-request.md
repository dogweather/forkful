---
title:                "Javascript: Versenden einer http-Anfrage"
simple_title:         "Versenden einer http-Anfrage"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Warum man HTTP-Anfragen senden sollte

Das Senden von HTTP-Anfragen ist ein wesentlicher Bestandteil der Webentwicklung. Durch das Senden von Anfragen von einem Client an einen Server können Daten abgerufen und dynamische Inhalte auf einer Webseite angezeigt werden. Ohne das Senden von HTTP-Anfragen wäre das Surfen im Internet nicht möglich.

# Wie man eine HTTP-Anfrage sendet

Um eine HTTP-Anfrage in Javascript zu senden, benötigen wir die Fetch API, die es uns ermöglicht, eine Anfrage an eine URL zu senden und die Antwort zu verarbeiten. Hier ist ein einfaches Beispiel, wie man eine HTTP-Anfrage mit der Fetch API sendet:

```Javascript
fetch('https://www.example.com')
  .then(response => response.text())
  .then(data => console.log(data))
```

Dieser Code sendet eine GET-Anfrage an die angegebene URL und gibt die Antwort als Text in der Konsole aus. Die Fetch API kann auch für POST-Anfragen verwendet werden, um Daten an den Server zu senden.

# Tiefere Einblicke in das Senden von HTTP-Anfragen

Es gibt verschiedene Arten von HTTP-Anfragen, die je nach Zweck und Verwendung unterschiedliche Methoden verwenden. GET-Anfragen werden verwendet, um Daten vom Server zu erhalten, während POST-Anfragen verwendet werden, um Daten an den Server zu senden. Weitere gängige Methoden sind PUT, DELETE oder PATCH.

Außerdem gibt es verschiedene Arten von HTTP-Statuscodes, die anzeigen, ob die Anfrage erfolgreich war oder ob ein Fehler aufgetreten ist. Zum Beispiel steht der Statuscode 200 für eine erfolgreiche Anfrage, während der Statuscode 404 einen Fehler anzeigt, wenn die angeforderte Ressource nicht gefunden werden konnte.

Weitere wichtige Aspekte beim Senden von HTTP-Anfragen sind die HTTP-Header, die zusätzliche Informationen über die Anfrage und die gewünschten Daten enthalten können, und die Verwendung von Asynchronität, um die Performance zu verbessern.

# Siehe auch

- [MDN Web Docs - Fetch API](https://developer.mozilla.org/de/docs/Web/API/Fetch_API)
- [HTTP-Statuscodes](https://developer.mozilla.org/de/docs/Web/HTTP/Status)
- [HTTP-Anfragemethoden](https://developer.mozilla.org/de/docs/Web/HTTP/Methods)