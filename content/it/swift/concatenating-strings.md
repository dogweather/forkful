---
title:                "Swift: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Concatenare stringhe è un'operazione fondamentale nella programmazione Swift. Unire più stringhe per creare una sola è spesso necessario per creare testo dinamico o complesso da mostrare all'utente. Senza la concatenazione di stringhe, i nostri programmi sarebbero molto più limitati.

## Come Fare
Per concatenare due o più stringhe in Swift, è possibile utilizzare l'operatore `+`. Ad esempio:

```Swift
let saluto = "Ciao"
let nome = "Maria"
let messaggio = saluto + ", " + nome + "!"
print(messaggio)

// Output:
// Ciao, Maria!
```

In questo esempio, abbiamo creato tre variabili contenenti rispettivamente il saluto, il nome e il messaggio completo concatenato utilizzando l'operatore `+`. Quando viene stampato il messaggio, le stringhe saranno unite insieme e verrà visualizzato "Ciao, Maria!".

Se invece si desidera concatenare una stringa ad una variabile temporale o costante, è possibile utilizzare la sintassi speciale `\(variabile)`. Ad esempio:

```Swift
let anno = 2021
let messaggioCompleanno = "Buon compleanno, \(anno)!"
print(messaggioCompleanno)

// Output: 
// Buon compleanno, 2021!
```

Ora, invece di dover aggiornare manualmente la stringa ogni anno, possiamo inserire la variabile `anno` all'interno del messaggio utilizzando la sintassi speciale.

## Approfondimento
Oltre all'utilizzo dell'operatore `+` e della sintassi speciale `\(variabile)`, esistono anche altri modi per concatenare stringhe in Swift. Ad esempio, è possibile utilizzare il metodo `append()` su una stringa vuota e passare le stringhe che si vogliono unire come argomento. Inoltre, ci sono anche funzioni specializzate come `stringByAppendingString()` per concatenare più stringhe in una sola volta. È importante ricordare di utilizzare tali metodi e funzioni in modo appropriato per evitare errori e risultati inaspettati.

## Vedi Anche
- [Documentazione ufficiale di Swift sulle stringhe](https://developer.apple.com/documentation/swift/string)
- [Tutorial su come concatenare stringhe in Swift](https://www.hackingwithswift.com/quick-start/swiftui/how-to-concatenate-strings-in-swiftui)