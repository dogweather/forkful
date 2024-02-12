---
title:                "Trovare la lunghezza di una stringa"
aliases:
- it/go/finding-the-length-of-a-string.md
date:                  2024-02-03T17:56:49.042378-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trovare la lunghezza di una stringa"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/finding-the-length-of-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Trovare la lunghezza di una stringa in Go riguarda il determinare il numero di caratteri che contiene. I programmatori eseguono regolarmente questa operazione per manipolare le stringhe in modo efficace, sia che si tratti di validazione, estrazione di sottostinghe, o semplicemente per imporre vincoli negli input degli utenti.

## Come fare:
In Go, le stringhe sono trattate come sequenze di byte immutabili. Puoi trovare la lunghezza di una stringa utilizzando la funzione integrata `len()`, che restituisce il numero di byte, non necessariamente il numero di caratteri. Ecco come usarla:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Usando len() per trovare la lunghezza in byte
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Lunghezza in Byte:", byteLength) // Output: Lunghezza in Byte: 13

	// Per ottenere con precisione il numero di caratteri o rune in una stringa
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Lunghezza in Rune:", runeLength) // Output: Lunghezza in Rune: 9
}
```
Il primo metodo che utilizza `len()` potrebbe non fornire sempre il risultato atteso poiché conta i byte. Per le stringhe che contengono caratteri non ASCII (come "世界"), si dovrebbe invece utilizzare `RuneCountInString` del pacchetto `unicode/utf8` per contare in modo accurato i punti di codice Unicode.

## Approfondimento
Prima di Go 1, non c'era una demarcazione rigorosa per gestire le stringhe come sequenze di byte rispetto a sequenze di caratteri. Dopo Go 1, l'adozione di UTF-8 come schema di codifica standard per le stringhe ha reso necessari approcci più chiari. La funzione `len()` funziona perfettamente per le stringhe ASCII, dove i caratteri sono rappresentati in un singolo byte. Tuttavia, man mano che le applicazioni Go diventavano più globali e cresceva la necessità di supportare un'ampia gamma di lingue e set di caratteri, l'approccio semplificato di `len()` mostrava limitazioni.

L'introduzione e l'uso di `utf8.RuneCountInString()` rispondono a queste limitazioni fornendo un modo per contare i caratteri Unicode effettivi (rune in terminologia Go). Questo metodo garantisce che il calcolo della lunghezza sia indipendente dalle specifiche di codifica di UTF-8, dove i caratteri possono occupare più byte.

Un approccio alternativo per attraversare e manipolare le stringhe, più in linea con l'etica di concorrenza ed efficienza di Go, potrebbe comportare il trattamento delle stringhe come slice di rune. Tuttavia, questo metodo richiede un passaggio di conversione e non risolve istantaneamente tutte le complessità di Unicode (ad esempio, i caratteri combinati).

In sintesi, mentre `len()` è adatto per la lunghezza in byte ed è efficiente per il testo ASCII, `utf8.RuneCountInString()` è una scelta più affidabile per un'applicazione compatibile a livello globale. Tuttavia, si incoraggiano gli sviluppatori a comprendere i compromessi in termini di prestazioni e uso della memoria che queste scelte comportano.
