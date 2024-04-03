---
date: 2024-01-20 18:00:31.510584-07:00
description: "Inviare una richiesta HTTP significa chiedere dati a un server web.\
  \ I programmatori lo fanno per interagire con servizi web, come recuperare dati\
  \ o\u2026"
lastmod: '2024-03-13T22:44:43.767247-06:00'
model: gpt-4-1106-preview
summary: Inviare una richiesta HTTP significa chiedere dati a un server web.
title: Inviare una richiesta http
weight: 44
---

## Come fare:
In Swift, possiamo utilizzare `URLSession` per gestire richieste HTTP. Ecco un esempio.

```Swift
import Foundation

// Creiamo l'URL della risorsa
if let url = URL(string: "https://api.miosito.it/dati") {
    
    // Creiamo una sessione
    let session = URLSession.shared
    
    // Creiamo la richiesta
    let task = session.dataTask(with: url) { data, response, error in
        // Controlliamo se ci sono errori
        if let error = error {
            print("Errore di richiesta: \(error)")
            return
        }
        
        // Controlliamo la risposta ed estraiamo i dati
        if let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200 {
            if let mimeType = httpResponse.mimeType, mimeType == "application/json",
               let data = data,
               let datiStringa = String(data: data, encoding: .utf8) {
                print("Dati ricevuti: \(datiStringa)")
            }
        } else {
            print("Problema con la risposta HTTP")
        }
    }
    
    // Eseguiamo la task
    task.resume()
}

```
Questo script fa una richiesta GET e stampa i dati ricevuti.

## Approfondimento
La richiesta HTTP è un concetto fondamentale del web dal 1991, data della prima proposta di HTTP. In Swift, URLSession è lo standard per inviare richieste, ma ci sono alternative come Alamofire, che rendono alcune cose più semplici. Importante: gestire i dati e le risposte asincronamente per non bloccare il thread principale, essenziale per l'esperienza utente.

## Vedi anche
- La documentazione di Swift su URLSession: https://developer.apple.com/documentation/foundation/urlsession
- Una guida ad Alamofire per richieste più complesse: https://github.com/Alamofire/Alamofire
