---
title:                "Kotlin: Leggere un file di testo"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché
Leggere un file di testo è un'operazione comune e fondamentale in ogni linguaggio di programmazione. In questo articolo, imparerai come farlo utilizzando il linguaggio di programmazione Kotlin.

## Come fare
Per leggere un file di testo in Kotlin, abbiamo bisogno di utilizzare la classe `File`, che si trova nel package `java.io.File`. Possiamo anche utilizzare il costruttore della classe `File` per creare un'istanza del file specificando il percorso del file come argomento. Una volta creato l'oggetto `File`, possiamo utilizzare il suo metodo `readText()` per leggere il contenuto del file come una stringa.

```Kotlin
val file = File("percorso_del_file.txt")
val contenuto = file.readText()
println(contenuto)
```

L'output sarà il contenuto del file di testo, che verrà stampato sulla console.

## Approfondimento
Leggere un file di testo non è solo una questione di ottenere il suo contenuto. Possiamo anche manipolare il contenuto del file come vogliamo. Ad esempio, possiamo suddividere il contenuto in parole utilizzando il metodo `split()` e quindi utilizzare il metodo `get()` per accedere a una specifica parola.

```Kotlin
val file = File("percorso_del_file.txt")
val contenuto = file.readText()
val contenutoDiviso = contenuto.split(" ")
println("La terza parola è: ${contenutoDiviso.get(2)}")
```

L'output sarà la terza parola del file di testo.

## Vedi anche
- Maggiori informazioni sulla classe `File` in Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html
- Tutorial di Kotlin per principianti: https://kotlinlang.org/docs/tutorials/getting-started.html
- Altri articoli su Kotlin: https://kotlinlang.org/community/