---
title:                "Go: Maiuscolare una stringa"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa è un'azione comune quando si lavora con i dati in Go. Ad esempio, potresti voler formattare una stringa per mostrarla in modo più leggibile o rendere la prima lettera maiuscola per scopi di presentazione. In questo articolo vedremo come capitalizzare una stringa utilizzando il linguaggio di programmazione Go e approfondiremo la logica dietro questa operazione.

## Come fare

Per capitalizzare una stringa in Go, puoi utilizzare la funzione incorporata `strings.ToUpper ()`. Questa funzione prende una stringa come parametro e restituisce una nuova stringa con tutte le lettere maiuscole. Ad esempio:

```Go
str := "esempio"
str = strings.ToUpper(str)
fmt.Println(str)
// Output: ESEMPIO
```

Se invece vuoi capitalizzare solo la prima lettera della stringa, puoi utilizzare la funzione `strings.Title ()`. Questa funzione restituisce una nuova stringa con la prima lettera maiuscola e tutte le altre lettere minuscole. Esempio:

```Go
str := "esempio"
str = strings.Title(str)
fmt.Println(str)
// Output: Esempio
```

È anche possibile utilizzare la funzione `strings.ToLower ()` per trasformare tutte le lettere di una stringa in minuscolo.

## Approfondimento

In Go, le stringhe sono immutabili, il che significa che non possono essere modificate una volta create. Quando viene chiamata una funzione come `strings.ToUpper ()` o `strings.Title ()`, viene creata una nuova stringa con il risultato e viene restituita. Ciò è diverso da altri linguaggi di programmazione in cui le stringhe possono essere modificate direttamente.

Inoltre, nel linguaggio Go, le stringhe sono codificate in UTF-8, il che significa che possono contenere caratteri di qualsiasi lingua, incluso l'italiano.

## Vedi anche

- [Documentazione ufficiale di Go sulle stringhe] (https://golang.org/pkg/strings/)
- [Tutorial su Go per programmatori Java] (https://www.it.boraji.com/2019/11/golang-to-java-developers-tutorial.html)
- [Come usare le stringhe in Go] (https://medium.com/rungo/strings-in-go-e10a6abb74c)