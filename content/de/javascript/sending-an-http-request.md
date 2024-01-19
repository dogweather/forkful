---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

### Was & Warum?

Das Senden einer HTTP-Anforderung ist ein Weg, Kommunikation zwischen Client und Server herzustellen, um beispielsweise Daten zu erhalten oder zu versenden. Dies ist essenziell für die Funktionalität webbasierter Anwendungen, da sie auf dem Austausch und der Manipulation von Daten basieren.

### Wie machst du das?

Wir werden den Fetch-API verwenden, um eine HTTP-Anforderung zu senden.

```Javascript
fetch('https://api.meineseite.de/daten')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch((error) => {
    console.error('Fehler:', error);
  });
```

In diesem Beispiel senden wir eine GET-Anforderung an 'https://api.meineseite.de/daten'. Dann verarbeiten wir die Antwort zu JSON, bevor wir sie ausgeben.

### Deep Dive

Die Verwendung von HTTP-Anforderungen zum Empfangen oder Senden von Daten ist kein neues Konzept. Es stammt aus der Anfangszeit des Webs, als sich Webseiten von statischen zu interaktiven entwickelten.

Es gibt Alternativen zum Fetch-API, wie die "XMLHttpRequest"-API. Doch Fetch bietet einen moderneren, leistungsfähigeren und flexibleren Ansatz.

Bezüglich der Implementierungsdetails ist zu beachten, dass HTTP-Anfragen asynchron sind. Das heißt, Ihr Code wird nicht angehalten, während er auf die Antwort vom Server wartet. Stattdessen wird Ihr Programm weiter ausgeführt und die Antwort wird verarbeitet, sobald sie verfügbar ist.

### Siehe auch

1. [MDN - Fetch API](https://developer.mozilla.org/de/docs/Web/API/Fetch_API)
2. [MDN - XMLHttpRequest](https://developer.mozilla.org/de/docs/Web/API/XMLHttpRequest)
3. [MDN - Asynchronous Javascript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Statements/async_function)