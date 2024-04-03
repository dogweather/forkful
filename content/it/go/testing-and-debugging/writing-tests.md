---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:02.346264-07:00
description: "Scrivere test in Go comporta la creazione di piccoli frammenti di codice\
  \ gestibili che validano la funzionalit\xE0 e il comportamento della tua applicazione.\u2026"
lastmod: '2024-03-13T22:44:42.912142-06:00'
model: gpt-4-0125-preview
summary: "Scrivere test in Go comporta la creazione di piccoli frammenti di codice\
  \ gestibili che validano la funzionalit\xE0 e il comportamento della tua applicazione."
title: Scrivere test
weight: 36
---

## Cosa & Perché?

Scrivere test in Go comporta la creazione di piccoli frammenti di codice gestibili che validano la funzionalità e il comportamento della tua applicazione. I programmatori scrivono test per assicurarsi che il loro codice funzioni come previsto in varie condizioni, per facilitare il refactoring e per aiutare a prevenire regressioni.

## Come fare:

In Go, i test sono tipicamente scritti nello stesso pacchetto del codice che testano. I file contenenti i test sono nominati con il suffisso `_test.go`. I test sono funzioni che prendono un puntatore all'oggetto testing.T (dal pacchetto `testing`) come argomento, e segnalano il fallimento chiamando metodi come `t.Fail()`, `t.Errorf()`, ecc.

Esempio di un semplice test per la funzione `Add` definita in `math.go`:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

File di test `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; want %d", result, expected)
    }
}
```

Esegui i tuoi test con il comando `go test` nella stessa directory dei tuoi file di test. Un esempio di output che indica un test superato sarebbe simile a:

```
PASS
ok      example.com/my/math 0.002s
```

Per i test basati su tabelle, che ti permettono di testare in modo efficiente varie combinazioni di input e output, definisci uno slice di struct che rappresentano i casi di test:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("got %d, want %d", ans, tt.expected)
            }
        })
    }
}
```

## Approfondimenti

Il framework di test di Go, introdotto in Go 1 insieme al linguaggio stesso, è stato progettato per integrarsi perfettamente con la toolchain di Go, riflettendo l'accento di Go sulla semplicità ed efficienza nello sviluppo software. A differenza di alcuni framework di test in altri linguaggi che si basano su librerie esterne o configurazioni complesse, il pacchetto `testing` integrato in Go offre un modo diretto per scrivere ed eseguire test.

Un aspetto interessante dell'approccio di Go al testing è il principio di convenzione rispetto alla configurazione che adotta, come il pattern di denominazione dei file (`_test.go`) e l'uso delle funzionalità della libreria standard rispetto alle dipendenze esterne. Questo approccio minimalista incoraggia gli sviluppatori a scrivere test, poiché la barriera all'ingresso è bassa.

Sebbene le strutture di test integrate in Go coprano molti aspetti, ci sono scenari in cui strumenti o framework di terze parti potrebbero offrire più funzionalità, come la generazione di mock, il fuzz testing, o i test in stile behavior-driven development (BDD). Librerie popolari come Testify o GoMock completano le capacità di test standard di Go, offrendo affermazioni più espressive o capacità di generazione di mock, che possono essere particolarmente utili in applicazioni complesse con molte dipendenze.

Nonostante l'esistenza di queste alternative, il pacchetto di test standard di Go rimane la pietra angolare per il testing in Go a causa della sua semplicità, performance e integrazione stretta con il linguaggio e la toolchain. Che gli sviluppatori scelgano o meno di arricchirlo con strumenti di terze parti, il framework di testing di Go fornisce una solida base per garantire la qualità e l'affidabilità del codice.
