---
title:                "Maiuscolizzare una stringa"
html_title:           "Kotlin: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che cosa e perché?
Capitalizzare una stringa significa trasformare la prima lettera di ogni parola in un carattere maiuscolo. I programmatori lo fanno per garantire una visualizzazione ordinata e professionale dei dati, specialmente quando si tratta di nomi, titoli e simili.

## Come fare:
Kotlin fornisce un metodo integrato, `capitalize()`, ottimizzato per i caratteri Unicode. Ecco come funziona:
```Kotlin
fun main() {
    val testo = "ciao mondo, benvenuto a kotlin"
    println(testo.capitalize())
}
```
Il risultato sarà: `Ciao Mondo, Benvenuto A Kotlin`
Se vuoi solo la prima lettera maiuscola, usa `replaceFirstChar()`:
```Kotlin
fun main() {
    val testo = "ciao mondo, benvenuto a kotlin"
    println(testo.replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString() })
}
```
Risultato: `Ciao mondo, benvenuto a kotlin`

## Approfondimenti
L'elevazione a maiuscolo delle stringhe era una pratica comune nel mondo della tipografia e della programmazione fin dalla loro nascita, con molteplici implementazioni e modi di eseguirlo per vari linguaggi.

Kotlin ha introdotto la funzione `capitalize()` per elevare la prima lettera di una parola a maiuscolo e la funzione `replaceFirstChar()` per operare solo sul primo carattere di una stringa. Sempre prestando particolare attenzione ai casi borderlines, come gli emoji, le stringhe vuote e i caratteri speciali.

Un'alternativa in Kotlin potrebbe essere l'utilizzo di `toUpperCase()`, ma bisogna fare attenzione poiché trasforma tutte le lettere della stringa in maiuscole, non solo la prima. Capire quale metodo utilizzare dipende dal tuo scenario specifico.

## Vedere Anche
Suggerisco di consultare questi link per maggiori dettagli e soluzioni:
1. [Documentazione ufficiale Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
2. [Kotlin capitalize in StackOverflow](https://stackoverflow.com/questions/50549955/how-to-capitalize-first-letter-of-a-string-in-kotlin)
3. [Comparazione tra capitalize() e toUpperCase()](https://stackoverflow.com/questions/57258916/capitalize-vs-to-uppercase-in-kotlin)