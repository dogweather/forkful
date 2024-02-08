---
title:                "Generazione di numeri casuali"
aliases:
- it/go/generating-random-numbers.md
date:                  2024-02-03T17:57:21.411954-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generazione di numeri casuali"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/generating-random-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Generare numeri casuali nella programmazione riguarda la creazione di una sequenza di numeri che non può essere prevista ragionevolmente meglio che a caso. I programmatori lo fanno per una miriade di motivi, inclusi simulazioni, giochi e applicazioni di sicurezza, dove l'imprevedibilità è chiave per la funzionalità o il segreto.

## Come fare:

In Go, i numeri casuali sono generati utilizzando il pacchetto `math/rand` per numeri pseudo-casuali o `crypto/rand` per numeri pseudo-casuali criptograficamente sicuri. Esploriamo entrambi.

### Usare `math/rand` per Numeri Pseudo-casuali

Prima, importare il pacchetto `math/rand` e il pacchetto `time` per inizializzare il generatore. Inizializzare assicura che si ottenga una sequenza diversa di numeri ad ogni esecuzione.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Un numero casuale:", rand.Intn(100)) // Genera un numero tra 0 e 99
}
```

Output di esempio: `Un numero casuale: 42`

### Usare `crypto/rand` per Numeri Pseudo-casuali Crittograficamente Sicuri

Per applicazioni più sensibili alla sicurezza, il pacchetto `crypto/rand` è adatto in quanto genera numeri casuali difficili da prevedere, rendendoli adatti per operazioni crittografiche.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("Un numero casuale sicuro:", n)
}
```

Output di esempio: `Un numero casuale sicuro: 81`

## Approfondimento

La differenza fondamentale tra i pacchetti `math/rand` e `crypto/rand` in Go deriva dalla loro fonte di entropia e dai loro casi d'uso previsti. `math/rand` genera numeri pseudo-casuali basati su un seme iniziale; quindi, la sequenza è deterministica e può essere prevista se il seme è noto. Ciò è adatto per scenari dove il massimo rendimento e non l'assoluta imprevedibilità è la preoccupazione principale, come le simulazioni o i giochi.

D'altra parte, `crypto/rand` deriva la casualità dal sistema operativo sottostante, rendendolo adatto per usi crittografici dove l'imprevedibilità è cruciale. Tuttavia, ciò comporta un costo in termini di prestazioni e complessità nel gestire i numeri che genera (come il trattamento del tipo `*big.Int` per gli interi).

Storicamente, la nozione di generazione di numeri casuali nei computer ha sempre danzato sul limite della vera "casualità", con i primi sistemi che dipendevano fortemente da algoritmi deterministici che mimavano la casualità. Man mano che i computer si sono evoluti, così hanno fatto questi algoritmi, incorporando fonti di entropia più sofisticate dai loro ambienti.

Nonostante questi progressi, la ricerca della perfetta casualità nel computing è intrinsecamente paradossale, data la natura deterministica dei computer stessi. Questo è il motivo per cui, per la maggior parte delle applicazioni dove la prevedibilità sarebbe dannosa, i numeri pseudo-casuali criptograficamente sicuri da fonti come `crypto/rand` sono l'alternativa migliore, nonostante il loro sovraccarico.

In sostanza, l'approccio di Go con due pacchetti distinti per la generazione di numeri casuali affronta elegantemente i compromessi tra performance e sicurezza, permettendo agli sviluppatori di scegliere in base alle loro specifiche esigenze.
