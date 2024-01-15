---
title:                "Eine http-Anfrage senden"
html_title:           "TypeScript: Eine http-Anfrage senden"
simple_title:         "Eine http-Anfrage senden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum
Um Daten von einem Server abzurufen oder zu senden, ist es notwendig, eine HTTP-Anfrage zu senden. Dies ist eine gängige Methode, um mit externen APIs oder Webanwendungen zu kommunizieren.

## Wie geht man vor?
Das Senden einer HTTP-Anfrage in TypeScript ist relativ einfach. Zunächst müssen wir die Bibliothek 'axios' installieren, die es uns ermöglicht, HTTP-Anfragen zu senden. Hier ist ein Beispielcode, der eine GET-Anfrage an einen Server sendet:

```TypeScript
import axios from 'axios';

axios.get('https://example.com/api')
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.log(error);
  });
```

Dieser Code verwendet die GET-Methode, um eine Anfrage an die URL "https://example.com/api" zu senden. Die Antwort des Servers wird in der Konsole ausgegeben. Um eine POST-Anfrage zu senden, können wir diese Methode anstelle von '.get' verwenden:

```TypeScript
axios.post('https://example.com/api', {
  name: 'Max',
  age: 25
})
.then(response => {
  console.log(response.data);
})
.catch(error => {
  console.log(error);
});
```

Hier wird zusätzlich ein Objekt mit den benötigten Daten an die Anfrage angehängt. Beachten Sie, dass wir in beiden Fällen eine '.then' und '.catch' Funktion verwenden, um die Serverantwort oder einen Fehler zu handhaben.

## Eintauchen in das Thema
HTTP-Anfragen können auch weitere Parameter enthalten, wie beispielsweise Header oder Query-Parameter. Um diese zu setzen, können wir ein sogenanntes 'config' Objekt an die Anfrage übergeben:

```TypeScript
axios.get('https://example.com/api', {
  headers: {
    Authorization: 'Bearer token123'
  },
  params: {
    category: 'books'
  }
})
.then(response => {
  console.log(response.data);
})
.catch(error => {
  console.log(error);
});
```

Dieses Beispiel zeigt, wie wir einen Autorisierungs-Header und einen Query-Parameter setzen können. Es ist wichtig zu beachten, dass jedes externe API seine eigenen Anforderungen an die HTTP-Anfragen stellt. Daher ist es ratsam, die Dokumentation des jeweiligen APIs zu konsultieren.

## Siehe auch
- Dokumentation von Axios: https://axios-http.com/
- Liste der HTTP-Statuscodes: https://de.wikipedia.org/wiki/HTTP-Statuscode
- Externe API-Dokumentation: <hier den Link zu einer spezifischen API einfügen>