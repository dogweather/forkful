---
title:                "Invio di una richiesta http con autenticazione di base"
html_title:           "Swift: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Che cos'è e perché lo facciamo?
In poche parole, inviare una richiesta HTTP con autenticazione di base è un modo per accedere ai dati protetti su una piattaforma web attraverso l'utilizzo di credenziali di accesso. I programmatori lo fanno per garantire la sicurezza dei dati sensibili e limitare l'accesso solo a coloro che hanno le credenziali corrette.

## Come fare:
Diamo un'occhiata a un esempio di codice per inviare una richiesta HTTP con autenticazione di base utilizzando Swift:

```Swift
// Definisci l'url della richiesta
let url = URL(string: "https://www.example.com/api/data")

// Crea l'oggetto richiesta
var request = URLRequest(url: url!)

// Imposta il metodo di richiesta
request.httpMethod = "GET"

// Aggiungi le credenziali di accesso
let username = "username"
let password = "password"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: String.Encoding.utf8)
let base64LoginString = loginData?.base64EncodedString()

request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// Invia la richiesta
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {                                                  
        return
    }
    
    // Gestisci la risposta qui...
    print("Response: \(response)")
}

task.resume()
```

Ecco un semplice esempio di output che si può ottenere utilizzando questo codice:

```Swift
Response: Optional(<NSHTTPURLResponse: 0x600002e77fc0> { URL: https://www.example.com/api/data } { Status Code: 200, Headers {
    "Content-Length" =     (
        119
    );
    "Content-Type" =     (
        "application/json"
    );
```

## Approfondimento:
L'autenticazione di base HTTP è stata introdotta per la prima volta nel 1999 come metodo semplice per proteggere i dati inviati tramite protocollo HTTP. Oggi, esistono altre forme di autenticazione più sicure, come OAuth 2.0, ma l'autenticazione di base è ancora comunemente utilizzata per le sue semplici e rapide implementazioni. Se hai bisogno di un livello maggiore di sicurezza, puoi esplorare altre opzioni di autenticazione.

## Vedi anche:
Per ulteriori informazioni su come inviare una richiesta HTTP con autenticazione di base in Swift, puoi consultare la documentazione ufficiale di Apple su [URLSession](https://developer.apple.com/documentation/foundation/urlsession). Inoltre, potresti trovare utile anche questo articolo di [HTTP Basic Authentication in Swift](https://medium.com/macoclock/http-basic-authentication-in-swift-345e988a4169) per una guida più dettagliata sull'argomento.