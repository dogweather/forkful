---
title:                "Concatenazione di stringhe"
html_title:           "Go: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?: 
La concatenazione di stringhe è un'operazione comune nel mondo della programmazione, dove due o più stringhe vengono unite per formare una nuova stringa più lunga. È spesso utilizzata per creare messaggi di output personalizzati o per manipolare e combinare dati. In Go, la concatenazione di stringhe viene eseguita utilizzando l'operatore "+".

## Come fare: 
Di seguito sono riportati alcuni esempi di codice che mostrano come eseguire la concatenazione di stringhe in Go utilizzando l'operatore "+". 

```
fmt.Println("Hello " + "World")
```
Output: `Hello World`

```
str1 := "Hello "
str2 := "World"
fmt.Println(str1 + str2)
```
Output: `Hello World`

```
num := 10
msg := "I have " + strconv.Itoa(num) + " apples."
fmt.Println(msg)
```
Output: `I have 10 apples.`

## Approfondimento:
La concatenazione di stringhe è stata introdotta in Go nel 2016 con la versione 1.10 del linguaggio. Prima di allora, gli sviluppatori dovevano utilizzare la funzione `fmt.Sprintf()` per concatenare le stringhe. Tuttavia, l'utilizzo dell'operatore "+" è molto più semplice e veloce.

In alternativa all'operatore "+", è anche possibile utilizzare il pacchetto `strings` che offre alcune funzioni utili per la manipolazione delle stringhe, come ad esempio `Join()` per unire una lista di stringhe.

Per quanto riguarda l'implementazione, l'operatore "+" è un'operazione non esclusiva di Go e può essere trovata in molti altri linguaggi di programmazione, come Java e Python.

## Vedi anche:
- Documentazione ufficiale di Go sulla concatenazione di stringhe: https://golang.org/ref/spec#Operators
- Esempi di codice su come utilizzare l'operatore "+" in Go: https://gobyexample.com/string-concatenation