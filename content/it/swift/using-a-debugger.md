---
title:                "Utilizzo di un debugger"
date:                  2024-01-26T04:10:40.698006-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un debugger"

category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/using-a-debugger.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Usare un debugger significa utilizzare strumenti specializzati per testare e ispezionare il tuo codice mentre viene eseguito. È importante perché ti permette di vedere cosa succede sotto il cofano, trovare bug e capire meglio il comportamento del tuo codice.

## Come fare:
Per utilizzare il debugger in Xcode (l'IDE per Swift), puoi impostare breakpoint, ispezionare variabili ed esaminare espressioni. Eccoti un esempio:

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

Imposta un breakpoint cliccando a sinistra di un numero di riga in Xcode ed esegui il programma. Quando il programma arriva al breakpoint, Xcode interrompe l'esecuzione. Ora puoi:

1. Controllare i valori delle variabili.
2. Fare "step over" (esegui la linea successiva) o "step into" (entra in una funzione) usando i controlli del debugger.
3. Aggiungere espressioni alla 'lista di osservazione' per monitorare i cambiamenti specifici di variabili o costanti.

Ecco cosa potresti vedere nell'area di debug:

```
(lldb) po number
5
(lldb) po result
120
```

## Approfondimento:
I debugger fanno parte del panorama della programmazione dagli anni '40, evolvendo da semplici sistemi di breakpoint a complesse esperienze guidate dall'interfaccia utente. Altre opzioni oltre al debugger integrato di Xcode includono strumenti di terze parti come LLDB (Low Level Debugger), che Xcode utilizza sotto il cofano. Alcuni addirittura fanno debug con istruzioni `print()` (affettuosamente noto come "debugging da cavernicolo"), ma questo risulta meno efficiente per grandi progetti o bug complessi. Quando usi un debugger, stai gestendo il controllo dell'esecuzione, l'introspezione in tempo reale e la manipolazione dei dati. Una profonda comprensione di questi principi contribuisce molto a un debugging efficiente.

## Vedi Anche:
- [Guida al Debugging di Xcode di Apple](https://developer.apple.com/documentation/xcode/debugging/)
- [Guida Rapida a LLDB](https://lldb.llvm.org/use/tutorial.html)
- [Tutorial sul Debugging di Swift di Ray Wenderlich](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
