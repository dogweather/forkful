---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?

Leggere gli argomenti da linea di comando significa ricevere input durante l'esecuzione del tuo programma da parte dell'utente. I programmatori lo fanno per rendere i loro programmi più interattivi e flessibili, adattabili alle esigenze dell'utente.

## Come si fa:

In Kotlin, leggere gli argomenti da linea di comando è semplice. Gli argomenti sono disponibili nell'array dei parametri della funzione `main()`. Ecco un esempio:

```Kotlin
fun main(args: Array<String>) {
   for (arg in args) {
       println(arg)
   }
}
```

Nell'esempio sopra, il nostro programma stampa tutti gli argomenti passati da linea di comando. Ad esempio, se eseguiamo `kotlin Main.kt arg1 arg2`, l'output sarà:

```
arg1
arg2
```

## Approfondimento

Kotlin, come la maggior parte dei linguaggi di programmazione moderni, supporta gli argomenti da linea di comando sin dalla sua nascita. Questa è una funzionalità che viene ereditata dai linguaggi di programmazione C e Unix Shell.

Esistono diverse alternative a questa funzionalità, ad esempio l'uso di file di configurazione o l'input da interfaccia grafica. Tuttavia, gli argomenti da linea di comando rimangono uno strumento potente, particolarmente utile per gli script e per l'automazione.

Dal punto di vista dell'implementazione, gli argomenti da linea di comando sono passati al programma come stringhe all'interno dell'array `args`. È responsabilità del programma interpretare questi dati nel modo corretto.

## Vedi Anche

- Per un'introduzione a Kotlin, consulta [Kotlin for Beginners](https://kotlinlang.org/docs/home.html).
- Per ulteriori informazioni su come lavorare con gli argomenti da linea di comando in Kotlin, consulta [Kotlin Documentation - Command Line Applications](https://kotlinlang.org/docs/command-line.html).