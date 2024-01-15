---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Swift: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Molti servizi web richiedono un'autenticazione per accedere ai dati e alle funzionalità. Utilizzare l'autenticazione di base per inviare una richiesta HTTP è un modo semplice e sicuro per verificare l'identità dell'utente.

## Come fare

Utilizzando la libreria Foundation di Swift, è possibile inviare una richiesta HTTP con autenticazione di base in poche righe di codice.

```
let username = "mario"
let password = "segreto"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: .utf8)
let base64LoginString = loginData!.base64EncodedString()
let url = URL(string: "https://www.example.com")
var request = URLRequest(url: url!)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {
        print(error?.localizedDescription ?? "Response error")
        return
    }
    let responseString = String(data: data, encoding: .utf8)
    print(responseString)
}
task.resume()
```

L'output della richiesta dovrebbe essere una stringa che contiene i dati a cui si è autenticati.

## Approfondimento

L'autenticazione di base utilizza il meccanismo di codifica Base64 per trasformare il nome utente e la password in una stringa. Questa stringa è poi aggiunta all'intestazione della richiesta HTTP come header "Authorization" con il prefisso "Basic". Il server riceve l'intestazione e decodifica la stringa, verificando se il nome utente e la password sono validi per accedere al servizio richiesto.

## Vedi anche

- [Apple Developer Documentation - URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [Base64Encoder - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/base64encoder)