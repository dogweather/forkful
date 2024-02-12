---
title:                "Inviare una richiesta http con autenticazione di base"
aliases:
- it/swift/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:48.404022-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http con autenticazione di base"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Le richieste HTTP con autenticazione di base sono una modalità standard per accedere a risorse protette da username e password. I programmatori le usano per comunicare in maniera sicura con server che richiedono credenziali di accesso.

## Come fare:
Ecco un modo semplice per inviare una richiesta HTTP con autenticazione di base in Swift:

```Swift
import Foundation

let username = "user"
let password = "pass"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()

var request = URLRequest(url: URL(string: "https://tuoserver.com")!)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {
        print(error ?? "Unknown error")
        return
    }

    if let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200 {
        let responseData = String(data: data, encoding: .utf8)
        print(responseData ?? "Non posso mostrare la risposta")
    } else {
        print("Errore nell'autenticazione o nella richiesta")
    }
}

task.resume()
```

Dovresti vedere la risposta del server nella console se l'autenticazione riesce.

## Approfondimento
L'autenticazione HTTP di base è uno dei modi più antichi per proteggere risorse web. È semplice ma non il più sicuro, dato che le credenziali sono codificate in Base64, ma non criptate. Alternativamente, puoi usare l'autenticazione bearer token o OAuth per maggiore sicurezza. Quando implementi l'autenticazione di base, è meglio usare HTTPS per proteggere le credenziali durante il trasporto. Inoltre, assicurati di gestire le credenziali con cura, non incorporandole direttamente nel codice.

## Vedi Anche
- [Autenticazione HTTP di base su MDN](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication)
- [Sicurezza delle comunicazioni HTTP in Swift con URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Guida alla codifica in Base64 in Swift](https://www.hackingwithswift.com/example-code/system/how-to-convert-between-base64-strings-and-nsdata)
