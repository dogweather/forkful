---
title:                "Swift: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo può essere utile per confrontare e manipolare i dati, come ad esempio nella ricerca di corrispondenze o nella formattazione di input. In Swift, ci sono diverse funzioni disponibili per eseguire questa conversione in modo efficiente.

## Come fare

Per convertire una stringa in minuscolo in Swift, si può utilizzare la funzione `lowercased()` su una variabile di tipo stringa. Ad esempio:

``` Swift
let stringa = "Ciao Mondo!"
let minuscola = stringa.lowercased()

print(minuscola)
// output: ciao mondo!
```

Si può anche utilizzare la funzione `lowercased()` direttamente su una costante o sulla stringa in input di una funzione o di un metodo.

``` Swift
let maiuscola = "HELLO WORLD!".lowercased()
print(maiuscola)
// output: hello world!

func stampaInMinuscolo(_ stringa: String) {
  print(stringa.lowercased())
}

stampaInMinuscolo("MIA FRASE IN MINUSCOLO")
// output: mia frase in minuscolo
```

## Approfondimenti

Quando si utilizza la funzione `lowercased()` in Swift, è importante ricordare che questa non solo converte tutte le lettere in minuscolo, ma anche tutti i caratteri speciali e di punteggiatura. Inoltre, in alcune lingue come il tedesco, potrebbe esserci una conversione errata delle lettere maiuscole se si utilizzano determinati caratteri speciali. Per ulteriori informazioni su come gestire queste situazioni, si consiglia di consultare la documentazione ufficiale di Apple su String e Character.

## Vedi anche
- Documentazione di Apple su String e Character: https://developer.apple.com/documentation/swift/string
- Altro articolo su come manipolare le stringhe in Swift: https://medium.com/better-programming/manipulating-strings-in-swift-c35732d46acc
- Esempi pratici di utilizzo della funzione `lowercased()` su Stack Overflow: https://stackoverflow.com/questions/42448787/lowercasing-a-string-in-swift-how-it-differs-from-objective-c