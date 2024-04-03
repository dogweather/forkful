---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:02.243526-07:00
description: "Lavorare con numeri complessi nella programmazione implica la manipolazione\
  \ di numeri che hanno sia una parte reale che una immaginaria, tipicamente\u2026"
lastmod: '2024-03-13T22:44:42.901299-06:00'
model: gpt-4-0125-preview
summary: Lavorare con numeri complessi nella programmazione implica la manipolazione
  di numeri che hanno sia una parte reale che una immaginaria, tipicamente espressi
  come `a + bi`.
title: Lavorare con i numeri complessi
weight: 14
---

## Come fare:
In Go, i numeri complessi sono gestiti utilizzando le funzioni incorporate `complex`, `real` e `imag`, insieme ai tipi `complex64` e `complex128` (rappresentanti rispettivamente numeri complessi a 64 bit e 128 bit). Ecco una guida rapida:

```go
package main

import (
	"fmt"
)

func main() {
	// Creazione di numeri complessi
	a := complex(2, 3) // 2+3i
	b := complex(1, -1) // 1-1i

	// Operazioni aritmetiche
	c := a + b
	fmt.Println("Addizione:", c) // Output: Addizione: (3+2i)

	d := a * b
	fmt.Println("Moltiplicazione:", d) // Output: Moltiplicazione: (5+1i)

	// Accesso alle parti reale e immaginaria
	parteReale := real(a)
	parteImmaginaria := imag(a)
	fmt.Printf("Parte reale: %.1f, Parte immaginaria: %.1f\n", parteReale, parteImmaginaria) // Output: Parte reale: 2.0, Parte immaginaria: 3.0

	// Il coniugato complesso e la magnitudine possono essere calcolati
	coniugato := complex(real(a), -imag(a)) // Manualmente
	fmt.Println("Coniugato di a:", coniugato) // Output: Coniugato di a: (2-3i)
}

```

Questo esempio copre le basi, ma c'è molto altro che si può fare con i numeri complessi, inclusa la sfruttamento del pacchetto `math/cmplx` per operazioni più avanzate come trovare la magnitudine, la fase, e molto altro.

## Approfondimento
Il concetto di numeri complessi risale al XVI secolo, ma ha ottenuto ampio riconoscimento e formalizzazione rigorosa solo nel XIX secolo. Nella programmazione informatica, i numeri complessi sono stati fondamentali per l'aritmetica complessa nei calcoli scientifici e ingegneristici fin dai primi giorni. L'approccio di Go ai numeri complessi, rendendoli cittadini di prima classe con supporto integrato e un supporto completo della libreria standard attraverso il pacchetto `math/cmplx`, spicca tra i linguaggi di programmazione. Questa decisione di progettazione riflette l'enfasi di Go sulla semplicità e le prestazioni.

Tuttavia, è importante notare che, sebbene lavorare con i numeri complessi in Go sia potente, potrebbe non essere sempre l'approccio migliore per tutte le applicazioni, soprattutto quelle che richiedono matematica simbolica o aritmetica ad alta precisione. Linguaggi e ambienti specializzati nel calcolo scientifico, come Python con librerie come NumPy e SciPy, o software come MATLAB, potrebbero offrire maggiore flessibilità e una gamma più ampia di funzionalità per applicazioni specifiche.

Detto questo, per la programmazione di sistemi e contesti in cui è cruciale integrare i calcoli con numeri complessi in un'applicazione più ampia e sensibile alle prestazioni, il supporto nativo di Go per i numeri complessi fornisce un'opzione unicamente efficiente.
