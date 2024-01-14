---
title:    "Go: Concatenazione di stringhe"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando si scrive un programma in Go, può essere necessario unire due o più stringhe insieme per formare una nuova stringa. Questo processo è chiamato concatenazione di stringhe ed è fondamentale per molte operazioni di programmazione.

## Come fare

In Go, la concatenazione di stringhe è molto semplice. Basta utilizzare l'operatore "+" tra due stringhe per unirle insieme. Ecco un esempio:

```Go
str1 := "Ciao "
str2 := "mondo!"
str3 := str1 + str2
fmt.Println(str3)
```

Output:
```
Ciao mondo!
```

È anche possibile aggiungere più di due stringhe utilizzando l'operatore "+" più volte come mostrato nell'esempio seguente:

```Go
str1 := "Go "
str2 := "è "
str3 := "un linguaggio di programmazione "
str4 := "molto potente!"
str5 := str1 + str2 + str3 + str4
fmt.Println(str5)
```

Output:
```
Go è un linguaggio di programmazione molto potente!
```

Inoltre, in Go è possibile utilizzare la funzione `fmt.Sprintf()` per formattare la stringa concatenata. Questo è utile quando si vogliono aggiungere variabili alle stringhe. Ecco un esempio:

```Go
name := "Mario"
age := 30
str := fmt.Sprintf("Ciao, mi chiamo %s e ho %d anni.", name, age)
fmt.Println(str)
```

Output:
```
Ciao, mi chiamo Mario e ho 30 anni.
```

## Approfondimento

È importante notare che utilizzare l'operatore "+" per concatenare stringhe in Go può essere costoso in termini di prestazioni. Per applicazioni in cui è importante ridurre il tempo di esecuzione, è consigliato utilizzare il pacchetto `strings` di Go che fornisce funzioni efficienti per la manipolazione di stringhe.

Inoltre, è importante comprendere che le stringhe in Go sono immutabili, il che significa che una volta create non possono essere modificate. Pertanto, ogni volta che si effettua una concatenazione di stringhe viene creato un nuovo oggetto stringa. In casi in cui è necessaria una performance ottimale, è consigliato utilizzare un tipo di dati diverso dalle stringhe, come ad esempio un buffer di byte, che può essere modificato direttamente.

## Vedi anche

- [Documentazione di Go sulla manipolazione di stringhe](https://golang.org/pkg/strings/)
- [Un approfondimento sulle performance della concatenazione di stringhe in Go](https://www.calhoun.io/concatenating-strings-efficiently-in-go/)