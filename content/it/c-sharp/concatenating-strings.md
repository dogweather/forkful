---
title:                "Concatenando stringhe"
html_title:           "C#: Concatenando stringhe"
simple_title:         "Concatenando stringhe"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Se ti stai avvicinando al mondo della programmazione in C#, è molto probabile che presto dovrai imparare il concetto di concatenazione di stringhe. Questa semplice operazione è uno dei fondamenti della programmazione e ti permette di unire più stringhe insieme per formarne una sola.

## Come fare

Per concatenare stringhe in C#, hai bisogno di utilizzare l'operatore "+" tra le varie stringhe che vuoi unire. Ad esempio:

```C#
string nome = "Marco";
string cognome = "Rossi";
string nomeCompleto = nome + " " + cognome;
```

In questo caso, la variabile "nomeCompleto" conterrà la stringa "Marco Rossi" ottenuta dall'unione delle stringhe "Marco" e "Rossi". Puoi anche concatenare più di due stringhe alla volta, semplicemente continuando ad aggiungere l'operatore "+" e le stringhe desiderate.

È importante notare che quando si uniscono stringhe in C#, è possibile anche inserire valori di altre variabili. Ad esempio:

```C#
string nome = "Maria";
int eta = 30;
string descrizione = "Ciao, mi chiamo " + nome + " e ho " + eta + " anni.";
```

In questo caso, la variabile "descrizione" conterrà la stringa "Ciao, mi chiamo Maria e ho 30 anni."

## Deep Dive

In C#, esiste anche la classe "StringBuilder" che può essere utilizzata per concatenare stringhe in modo più efficiente rispetto all'utilizzo dell'operatore "+". Questa classe è particolarmente utile quando si desidera unire un gran numero di stringhe, perché evita il riallocazione continua di memoria che si verifica utilizzando l'operatore "+".

Inoltre, è importante tenere presente che quando si concatenano molte stringhe in C#, può verificarsi un fenomeno chiamato "boxing", che è un processo che rallenta notevolmente l'esecuzione del codice. Per evitare questo problema, si consiglia di utilizzare la classe "StringBuilder" o di utilizzare altri metodi come l'utilizzo di formattazione di stringhe.

## Vedi anche

- [Documentazione ufficiale su come concatenare stringhe in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/concatenating-strings)
- [Articolo su come gestire correttamente la concatenazione di stringhe in C#](https://www.c-sharpcorner.com/article/string-concatenation-in-C-Sharp/)
- [Video tutorial su come utilizzare la classe "StringBuilder" per concatenare stringhe in C#](https://www.youtube.com/watch?v=6RQcEJq3r30)