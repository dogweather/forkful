---
title:                "Kotlin: Verifica dell'esistenza di una cartella"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
Spesso, durante la programmazione, ci troviamo a dover gestire diverse directory e file. Per evitare errori e migliorare l'esperienza utente, può essere utile controllare se una directory esiste prima di eseguire delle operazioni su di essa.

## Come Fare
Per controllare se una directory esiste in Kotlin, possiamo utilizzare il metodo `exists()` della classe `File`. Iniziamo importando la classe `File` nel nostro progetto:
```Kotlin
import java.io.File
```
Successivamente, creiamo un oggetto di tipo `File` a cui passiamo il percorso della directory che vogliamo controllare:
```Kotlin
val directory = File("/percorso/della/directory")
```
Infine, possiamo utilizzare il metodo `exists()` per verificare se la directory esiste:
```Kotlin
if(directory.exists()){
    println("La directory esiste!")
} else {
    println("La directory non esiste.")
}
```
Se la directory esiste, verrà stampato a schermo il messaggio "La directory esiste!", altrimenti verrà stampato "La directory non esiste.".

## Approfondimento
A volte potremmo voler sapere non solo se una directory esiste, ma anche se è una directory vera e propria e non un file o un altro tipo di elemento del file system. In tal caso, possiamo utilizzare il metodo `isDirectory()` della classe `File`:
```Kotlin
if(directory.exists() && directory.isDirectory()){
    println("La directory esiste ed è una vera directory.")
} else if(directory.exists() && !directory.isDirectory()){
    println("Il percorso specificato corrisponde a un file o ad un altro elemento del file system.")
} else {
    println("La directory non esiste.")
}
```

## Vedi Anche
- [Documentazione ufficiale di Kotlin su File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Articolo su come gestire i file e le directory in Kotlin](https://www.baeldung.com/kotlin/working-with-files-and-directories)
- [Esempi di utilizzi più avanzati del metodo `exists()`](https://www.javatpoint.com/kotlin-working-with-file-io#exist-method)