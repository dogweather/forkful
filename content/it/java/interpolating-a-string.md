---
title:                "Interpolare una stringa"
html_title:           "Java: Interpolare una stringa"
simple_title:         "Interpolare una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

L'interpolazione di una stringa è un modo per inserire valori dinamici all'interno di una stringa fissa. Questo è utile per creare stringhe personalizzate in base a variabili o input dall'utente. I programmatori utilizzano l'interpolazione di stringhe per rendere il loro codice più flessibile e dinamico.

## Come fare:

Ecco un esempio di interpolazione di stringa in Java:

```Java
String nome = "Mario";
int eta = 30;

System.out.println("Ciao, mi chiamo ${nome} e ho ${eta} anni.");
```

Questo codice produce l'output: "Ciao, mi chiamo Mario e ho 30 anni." Come puoi vedere, il valore delle variabili viene inserito all'interno della stringa tra parentesi graffe.

Puoi anche utilizzare l'interpolazione di stringa per concatenare più variabili o eseguire operazioni all'interno della stringa. Ecco un altro esempio:

```Java
int numero1 = 5;
int numero2 = 10;
int risultato = numero1 + numero2;

System.out.println("La somma di ${numero1} e ${numero2} è ${risultato}.");
```

L'output sarà: "La somma di 5 e 10 è 15."

## Approfondimento:

L'interpolazione di stringa è stata introdotta in Java 15 come alternativa alla concatenazione di stringhe con l'operatore `+`. Questo rende il codice più leggibile e facile da mantenere. Inoltre, è possibile utilizzare l'interpolazione di stringa anche con caratteri speciali o stringhe di formato.

Un'altra alternativa all'interpolazione di stringa è l'utilizzo della classe `String.format()`, che consente di impostare un formato specifico per ogni argomento della stringa.

Per quanto riguarda l'implementazione, l'interpolazione di stringa utilizza una combinazione di metodi `StringBuilder` e `String.format()` per creare la stringa finale con i valori inseriti al loro posto.

## Vedi anche:

- [Java String.format() documentation] (https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#format(java.lang.String,java.lang.Object...))
- [Java 15 release notes] (https://www.oracle.com/java/technologies/javase/15-relnote-issues.html#JDK-8240594)