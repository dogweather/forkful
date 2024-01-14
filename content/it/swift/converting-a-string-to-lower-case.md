---
title:    "Swift: Convertire una stringa in minuscolo"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché
Perché convertire una stringa in minuscolo? Ci possono essere molte ragioni, ad esempio per rendere uniforme il testo, confrontare due stringhe o per eseguire operazioni di ricerca più precise.

## Come fare
Per convertire una stringa in minuscolo in Swift, puoi utilizzare il metodo `lowercased()` come mostrato nell'esempio seguente:

```Swift
let stringaIniziale = "BENVENUTI IN ITALIA"
let stringaMinuscola = stringaIniziale.lowercased()

print(stringaMinuscola) // output: benvenuti in italia
```

Puoi anche specificare la lingua di riferimento per la conversione, il che risulta utile per alcune lingue che hanno caratteri di codifica diversi. Ad esempio, per la lingua italiana, puoi utilizzare il seguente codice:

```Swift
let stringaIniziale = "BENVENUTI IN ITALIA"
let stringaMinuscola = stringaIniziale.lowercased(with: Locale(identifier: "it_IT"))

print(stringaMinuscola) // output: benvenuti in italia
```

Se necessario, puoi anche convertire una stringa in minuscolo senza utilizzare un metodo specifico, semplicemente utilizzando il metodo `capitalized` e poi rendendo la prima lettera minuscola.

## Approfondimento
La conversione di una stringa in minuscolo può sembrare un'operazione banale, ma in realtà coinvolge molte operazioni dietro le quinte. Per esempio, le lettere accentate possono essere trasformate in caratteri diversi o addirittura in combinazioni di più caratteri, a seconda della lingua o delle impostazioni di codifica utilizzate.

Inoltre, alcune lingue hanno regole specifiche per il caso, come l'inglese dove i nomi propri sono scritti con la prima lettera maiuscola. Perciò, è importante conoscere bene la lingua con cui si sta lavorando per evitare errori o risultati inaspettati.

## Vedi anche
- La documentazione ufficiale di Swift per il metodo `lowercased()`: [https://developer.apple.com/documentation/foundation/nsstring/1390491-lowercased](https://developer.apple.com/documentation/foundation/nsstring/1390491-lowercased)
- Un tutorial su come lavorare con le stringhe in Swift: [https://learnappmaking.com/working-with-strings-swift-how-to/](https://learnappmaking.com/working-with-strings-swift-how-to/)
- Un esempio di utilizzo del metodo `lowercased()` in una app Swift: [https://theswiftdev.com/how-to-add-a-search-bar-to-filter-results-in-swift/](https://theswiftdev.com/how-to-add-a-search-bar-to-filter-results-in-swift/)