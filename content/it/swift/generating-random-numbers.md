---
title:    "Swift: Generazione di numeri casuali"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

L'utilizzo di numeri casuali è fondamentale in molti aspetti della programmazione, come nella creazione di giochi o nell'esecuzione di test casuali. Anche per scopi di sicurezza, è importante essere in grado di generare numeri casuali affidabili.

## Come fare

Per generare numeri casuali in Swift, è possibile utilizzare la funzione `arc4random_uniform` del framework `Foundation`. Questa funzione accetta un parametro intero e restituisce un numero intero casuale tra 0 e il parametro meno 1.

```
Swift
let random = arc4random_uniform(10)
print(random)
// Output: un numero casuale tra 0 e 9
```

In alternativa, è possibile utilizzare il metodo `random` della classe `Int` per generare un numero casuale compreso tra due valori specifici:

```
Swift
let random = Int.random(in: 1...10)
print(random)
// Output: un numero casuale tra 1 e 10
```

## Approfondimento

La generazione di numeri casuali non è una semplice operazione e richiede una certa attenzione. In Swift, la funzione `arc4random_uniform` utilizza un generatore di numeri pseudo-casuali a 32 bit chiamato Mersenne Twister. Questo generatore è estremamente potente e la probabilità di ripetere lo stesso numero è molto bassa, tuttavia non è completamente casuale.

Se è necessario una maggiore sicurezza nella generazione dei numeri casuali, è consigliabile utilizzare il framework `CryptoKit` introdotto in iOS 13 e macOS 10.15. Questo framework offre funzioni per la generazione di numeri casuali crittograficamente sicuri.

## Vedi anche

- [Docs: Generazione di numeri casuali in Swift](https://developer.apple.com/documentation/swift/int/2995600-random)
- [Articolo: Generazione di numeri casuali in Swift con CryptoKit](https://www.hackingwithswift.com/example-code/system/how-to-generate-random-number-in-swift-using-cryptokit)