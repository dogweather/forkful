---
title:                "Inviare una richiesta http."
html_title:           "Swift: Inviare una richiesta http."
simple_title:         "Inviare una richiesta http."
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
In poche parole, inviare una richiesta HTTP significa comunicare con un server web per ottenere o mandare dati. I programmatori lo fanno per creare applicazioni web, servizi RESTful, e molto altro.

## Come fare:
Un esempio di codice semplice per inviare una richiesta GET ad un sito web:

```Swift
let url = URL(string: "https://www.example.com")!
let request = URLRequest(url: url)
let session = URLSession.shared

session.dataTask(with: request) { data, response, error in
    if let data = data, let response = response {
        print("Il server ha risposto con status code \(response.statusCode)")
        print("Il contenuto della risposta è \(String(data: data, encoding: .utf8)!)")
    } else if let error = error {
        print(error.localizedDescription)
    }
}.resume()
```

Output:

```
Il server ha risposto con status code 200
Il contenuto della risposta è <html>...</html>
```

## Profondità Inesplorata:
La richiesta HTTP è una tecnica chiave nella programmazione web, utilizzata per comunicare con server da applicazioni client. Ci sono alternative come WebSocket e TCP sockets, ma sono più complesse da implementare. In Swift, è possibile utilizzare la libreria nativa URLSession o librerie di terze parti come Alamofire per gestire la comunicazione HTTP.

## Vedi anche:
- [Introduzione a HTTP](https://developer.mozilla.org/it/docs/Web/HTTP/Overview)
- [Documentazione URLSession di Apple](https://developer.apple.com/documentation/foundation/urlsession)
- [Documentazione Alamofire](https://github.com/Alamofire/Alamofire)