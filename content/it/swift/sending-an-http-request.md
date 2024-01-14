---
title:                "Swift: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

##Perché

Molti sviluppatori si trovano a dover inviare richieste HTTP tramite codice Swift. Questo può essere necessario per comunicare con API esterne, recuperare dati da un server o qualsiasi altra azione che richieda una comunicazione tramite protocollo HTTP.

##Come fare

Per inviare una richiesta HTTP in Swift, è innanzitutto necessario utilizzare la classe `URLSession` per aprire una sessione di comunicazione tra l'applicazione e il server. Successivamente, è possibile creare una richiesta utilizzando la classe `URLRequest` e impostare il metodo HTTP desiderato, l'URL di destinazione e eventuali parametri o body della richiesta. Infine, occorre utilizzare un'istanza di `URLSessionDataTask` per eseguire la richiesta e ottenere la risposta dal server.

Ecco un esempio di codice Swift per inviare una richiesta GET e stampare la risposta ottenuta:

```Swift
let session = URLSession.shared
let url = URL(string: "https://www.exemplo.com/api/data")!

let task = session.dataTask(with: url) { (data, response, error) in
    guard let data = data else {
        print("Errore nella risposta del server: \(String(describing: error))")
        return
    }
    
    print("Risposta del server:")
    print(String(data: data, encoding: .utf8))
}

task.resume()
```

L'output di questo codice sarà una stampa del body della risposta del server, se tutto è andato a buon fine.

##Approfondimento

La classe `URLRequest` permette di impostare molteplici opzioni per una richiesta HTTP, tra cui gli header e i parametri del body. Inoltre, è possibile specificare il timeout della richiesta, gestire eventuali errori e impostare certificati di sicurezza per le richieste HTTPS. In caso di richieste che richiedono autenticazione, è possibile aggiungere le credenziali al `URLRequest` utilizzando la classe `URLCredential`.

##Vedi anche

- [Documentazione ufficiale di Apple su URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [Esempi di utilizzo di URLSession in Swift](https://www.swiftbysundell.com/articles/urlsession-in-swift/)
- [Come gestire le credenziali nelle richieste HTTP in Swift](https://medium.com/@darthpelo/http-auth-with-swift-3-16e15f6c8a7b)