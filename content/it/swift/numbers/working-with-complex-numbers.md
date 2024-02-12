---
title:                "Lavorare con i numeri complessi"
aliases:
- /it/swift/working-with-complex-numbers.md
date:                  2024-01-26T04:45:41.622823-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i numeri complessi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

# Cosa & Perché?
I numeri complessi hanno una parte reale e una parte immaginaria (come 3 + 4i). I programmatori li utilizzano in Swift per compiti come l'elaborazione di segnali, la risoluzione di certi problemi matematici e la simulazione fisica.

## Come fare:
Swift non ha un supporto integrato per i numeri complessi, ma possiamo crearne uno nostro:

```Swift
struct NumeroComplesso {
    var reale: Double
    var immaginario: Double
    
    func add(_ altro: NumeroComplesso) -> NumeroComplesso {
        return NumeroComplesso(reale: reale + altro.reale, immaginario: immaginario + altro.immaginario)
    }
    
    // Metodi aggiuntivi come sottrazione, moltiplicazione, ecc.
}

let primo = NumeroComplesso(reale: 2, immaginario: 3)
let secondo = NumeroComplesso(reale: 1, immaginario: 4)
let risultato = primo.add(secondo)
print("Risultato: \(risultato.reale) + \(risultato.immaginario)i")
// Output di esempio: Risultato: 3.0 + 7.0i
```

## Approfondimento
I numeri complessi sono comparsi nel XVI secolo nelle equazioni algebriche. Sono essenziali nella meccanica quantistica, nella teoria del controllo e in molti altri campi. Swift di Apple non ha una libreria standard per i numeri complessi, a differenza di linguaggi come Python o C++. Alternative alla creazione propria includono l'uso del pacchetto Numerics che include il supporto ai numeri complessi o l'incapsulamento della libreria complessa di C++ con l'interoperabilità di Swift.

## Vedi Anche
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
