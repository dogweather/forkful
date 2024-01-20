---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Invio di richieste HTTP con autenticazione di base in Swift

## Cosa & Perché?
L'invio di una richiesta HTTP con autenticazione di base in Swift consente all'applicazione di interagire con i servizi web in modo sicuro. E' un metodo comunemente usato dai programmatori per proteggere le informazioni sensibili inviate attraverso la rete.

## Come fare:
Ecco un breve esempio di come si può inviare una richiesta HTTP con autenticazione di base in Swift:

```Swift
import Foundation

let username = "username"
let password = "password"
let loginString = "\(username):\(password)"

if let data = loginString.data(using: .utf8) {
    let credentials = data.base64EncodedString()

    var request = URLRequest(url: URL(string: "https://your-api.com/")!)
    request.httpMethod = "POST"

    request.setValue("Basic \(credentials)", forHTTPHeaderField: "Authorization")

    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        guard let data = data else {
            print(String(describing: error))
            return
        }

        print(String(data: data, encoding: .utf8)!)
    }

    task.resume()
}
```
Output di esempio:

```Swift
Result: Your requested data
```

## Approfondimenti
La tecnica di invio delle richieste HTTP con autenticazione di base risale agli albori del web, quando i metodi di autenticazione erano molto limitati. 

Un'alternativa comune all'autenticazione di base è l'uso dei token JWT (Json Web Tokens), che offre un livello di sicurezza più avanzato.

In Swift, l'invio di queste richieste si realizza attraverso la classe URLSession, che gestisce la connessione in background e offre diversi livelli di configurazione.

## Vedi anche
Per ulteriori informazioni o approfondimenti su questo argomento:

1. Documentazione Apple su URLSession: [https://developer.apple.com/documentation/foundation/urlsession](https://developer.apple.com/documentation/foundation/urlsession)
2. Tutorial su come inviare richieste HTTP in Swift: [https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started](https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started)
3. RFC 7617 - Autenticazione di base HTTP: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
4. Guida sull'utilizzo dei JWT in Swift: [https://auth0.com/docs/quickstart/native/ios-swift](https://auth0.com/docs/quickstart/native/ios-swift)