---
title:    "Kotlin: Lettura degli argomenti della linea di comando"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Ciao lettori! Se siete appassionati di programmazione, probabilmente avete già sentito parlare delle opzioni di riga di comando (command line arguments). Ma perché dovreste interessarvi a questo argomento? La risposta è semplice: imparare come leggere e gestire correttamente i parametri di riga di comando vi aiuterà a creare applicazioni più efficienti e ottenere una maggiore flessibilità nel vostro codice. Quindi, se volete migliorare le vostre abilità di programmazione, continuate a leggere!

## Come

Per iniziare, vediamo un esempio di codice in Kotlin che ci permette di leggere i parametri di riga di comando:

```Kotlin 
fun main(args: Array<String>) {
    if (args.size > 0) {
        println("Il primo parametro è: ${args[0]}")
    } else {
        println("Nessun parametro inserito.")
    }
}
```

Questo semplice programma ci mostrerà il primo parametro presente nella riga di comando, se presente. Eseguendolo con i seguenti comandi: 

- `kotlin Main.kt`  (senza argomenti) 
- `kotlin Main.kt test` (con il parametro "test") 

Otterremo rispettivamente i seguenti output: 

- "Nessun parametro inserito."
- "Il primo parametro è: test"

Come potete notare, il parametro letto dalla riga di comando viene passato come un array alla funzione `main()` e possiamo accedervi utilizzando l'operatore di indice `[]`.

Se siete pronti a sperimentare un po' di più, potete provare a gestire gli errori in caso di parametri non validi o utilizzare librerie esterne come "argparser" per una gestione più avanzata dei comandi di riga.

## Deep Dive

Per una comprensione più completa dei parametri di riga di comando, è importante che comprendiate come sono strutturati. I parametri sono inseriti dopo il nome del programma da eseguire nella riga di comando, separati da spazi. Se il parametro contiene uno spazio, è necessario incluirlo tra virgolette. Inoltre, i comandi di riga di comando possono essere di diversi tipi, come opzioni, argomenti o a switch.

Le opzioni sono parametri che iniziano con un trattino singolo "-" o doppio "--" e possono essere seguite da un valore. Le opzioni di tipo switch non hanno alcun valore e vengono semplicemente attivate o disattivate. Gli argomenti, invece, non iniziano con un trattino e forniscono un valore specifico al programma.

Sapere come identificare e distinguere i vari tipi di parametri di riga di comando vi aiuterà a utilizzarli in modo più efficace nei vostri progetti futuri.

## Vedere anche

Per ulteriori informazioni sugli argomenti trattati in questo articolo, date un'occhiata a questi utili link:

- [Documentazione di Kotlin](https://kotlinlang.org/docs/reference/command-line.html)
- [Tutorial di command line arguments in Kotlin](https://www.javatpoint.com/kotlin-command-line-arguments)
- [Libreria argparser per gestione avanzata di command line arguments](https://github.com/xenomachina/kotlin-argparser)

Grazie per avermi seguito in questo viaggio alla scoperta dei command line arguments in Kotlin. Continuate a esplorare e a sperimentare per diventare sempre più esperti nella vostra programmazione! Arrivederci alla prossima!