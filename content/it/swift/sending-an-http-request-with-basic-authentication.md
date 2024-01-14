---
title:                "Swift: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Sending a basic authentication HTTP request may be necessary for accessing protected APIs or websites that require authentication. This allows for secure communication between the client and server, ensuring that only authorized users are granted access.

## Come

Per inviare una richiesta HTTP con autenticazione di base in Swift, è necessario seguire alcuni semplici passaggi:

1. Importare la libreria `Foundation` nell'inizio del file:
```
import Foundation
```

2. Creare una costante con le credenziali di autenticazione:
```
let loginString = String(format: "%@:%@", username, password)
```

3. Codificare la stringa utilizzando Base64:
```
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()
```

4. Creare la testata Authorization da aggiungere alla richiesta:
```
var headers = [String: String]()
headers["Authorization"] = "Basic \(base64LoginString)"
```

5. Utilizzare `URLSession` per creare e inviare la richiesta HTTP:
```
let request = URLRequest(url: url)
request.allHTTPHeaderFields = headers
let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    guard let data = data, error == nil else {
        print("Error: \(error?.localizedDescription)")
        return
    }
    
    if let response = response as? HTTPURLResponse, response.statusCode == 200 {
        print("Success! Data received: \(data)")
    } else {
        print("Error: Invalid response")
    }
}
```

6. Eseguire la richiesta:
```
task.resume()
```

Il codice sopra illustra come creare una richiesta con autenticazione di base utilizzando Swift. È importante notare che è necessario fornire un username e password validi per l'autenticazione.

## Profondità

Quando si invia una richiesta con autenticazione di base, è fondamentale ricordare che la stringa di login deve essere codificata in Base64. Questo è un sistema di codifica che converte i dati binari in un formato testuale, rendendoli facilmente trasferibili su una rete. In questo caso, la stringa di login codificata viene aggiunta alla testata Authorization, che viene quindi inviata al server.

Inoltre, è possibile specificare un timeout per la richiesta in modo da gestire eventuali errori o rallentamenti di rete. Questo può essere fatto impostando il valore della proprietà `timeoutInterval` dell'`URLRequest`.

## Vedi Anche

- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#HTTP_Basic_Authentication)
- [Swift Foundation Framework](https://developer.apple.com/documentation/foundation)
- [URLSession](https://developer.apple.com/documentation/foundation/urlsession)