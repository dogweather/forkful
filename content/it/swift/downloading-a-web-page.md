---
title:                "Scaricare una pagina web"
html_title:           "Swift: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scaricare una pagina web significa ottenere il suo contenuto in formato testo. I programmatori fanno questo per elaborare i dati della pagina o per integrarli in altre applicazioni.

## Come fare:
Ecco un esempio semplice di come scaricare una pagina web utilizzando Swift. Questo codice utilizzerà URLSession e completionHandler per ottenere il contenuto della pagina in modo asincrono.

```
let urlString = "https://www.example.com"

if let url = URL(string: urlString) {
    let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
        if let error = error {
            print("Errore: \(error)")
        } else if let data = data {
            if let htmlString = String(data: data, encoding: .utf8) {
                print(htmlString)
            }
        }
    }
    task.resume()
}
```
L'output di questo codice sarà il contenuto della pagina come testo nella console (assicurati di eseguire il codice su un dispositivo fisico o emulatore per vedere l'output).

## Approfondimento:
La pratica di scaricare una pagina web ha radici nel passato, quando i computer non erano sempre connessi a Internet e quindi era necessario scaricare i contenuti delle pagine per poterli visualizzare offline. Ora, con la diffusione di Internet, questa pratica è rimasta utile per scopi di scraping dei dati o integrazione con altre piattaforme. Esistono anche librerie e framework come Alamofire che semplificano il processo di scaricamento di una pagina web in Swift.

## Vedi anche:
- [Apple Developer Documentation - URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Alamofire](https://github.com/Alamofire/Alamofire)