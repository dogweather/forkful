---
title:                "Convertire una stringa in minuscolo"
html_title:           "Swift: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Convertire una stringa in minuscolo è il processo di trasformare ogni lettera in una stringa in una minuscola, ovvero una lettera di alfabeto più piccola. I programmatori spesso eseguono questa operazione per uniformare il formato del testo o per effettuare confronti di stringhe senza considerare le maiuscole e le minuscole.

## Come fare:
```Swift
let stringa = "Swift è un linguaggio di programmazione"
let stringaInMinuscolo = stringa.lowercased()
print(stringaInMinuscolo)
```
**Output:**
swift è un linguaggio di programmazione

## Approfondimento:
Ci sono molte ragioni per cui potresti voler convertire una stringa in minuscolo. Uno dei motivi può essere quello di fare corrispondere una stringa in un database con un input dell'utente, soprattutto se l'utente ha inserito il testo senza prestare attenzione alle maiuscole e alle minuscole.
Inoltre, nei sistemi non case-sensitive, la conversione in minuscolo garantisce il corretto funzionamento delle operazioni di ricerca o di confronto di stringhe.

**Alternative:**
In alcuni casi, potrebbe essere necessario convertire una stringa in maiuscolo anziché in minuscolo. In questo caso, si può usare il metodo ```uppercased ()```.
Oppure, se si lavora con i caratteri Unicode, ci sono altre opzioni disponibili per la conversione di stringhe in maiuscolo o minuscolo che tengano conto dei caratteri accentati.

**Dettagli Implementativi:**
Il metodo ```lowercased ()``` è un metodo della classe String in Swift che restituisce una copia della stringa originale con tutti i caratteri in minuscolo. Questo metodo non modifica la stringa originale, ma ne crea una nuova.
Nella versione più recente di Swift, esiste anche il metodo ```localizedLowercase``` per la conversione dei caratteri di una stringa in minuscolo in base alle impostazioni regionali specifiche.

## Vedi Anche:
Per ulteriori informazioni su stringhe e conversione di case in Swift, consulta la documentazione ufficiale di Apple su [String](https://developer.apple.com/documentation/swift/string) e [Character Properties](https://developer.apple.com/documentation/swift/character-properties).