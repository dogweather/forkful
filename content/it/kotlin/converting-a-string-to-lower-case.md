---
title:    "Kotlin: Trasformare una stringa in minuscolo"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo è un'operazione comune nella programmazione Kotlin, soprattutto quando si lavora con input utente o con comparazioni di stringhe. In questo articolo, imparerai come eseguire questa operazione facilmente.

## Come fare

Per convertire una stringa in minuscolo in Kotlin, è possibile utilizzare il metodo `toLowerCase()` come mostrato di seguito:

```Kotlin
val stringa = "CIAO MONDO!"
val stringaMinuscola = stringa.toLowerCase()

println(stringaMinuscola) // output: ciao mondo!
```

In questo esempio, abbiamo dichiarato una variabile `stringa` con il valore "CIAO MONDO!" e utilizzato il metodo `toLowerCase()` per convertirla in minuscolo. Quindi abbiamo stampato l'output utilizzando il metodo `println()`.

Puoi anche utilizzare il metodo `toLowerCase()` direttamente su una stringa senza dichiarare una nuova variabile:

```Kotlin
println("HELLO WORLD!".toLowerCase()) // output: hello world!
```

Inoltre, se vuoi convertire solo una parte della stringa in minuscolo, puoi specificare l'indice del carattere fino al quale vuoi eseguire la conversione:

```Kotlin
val frase = "Il gatto nero"
val fraseMinuscola = frase.substring(3).toLowerCase()

println(fraseMinuscola) // output: gatto nero
```

In questo caso, abbiamo utilizzato il metodo `substring()` per specificare l'indice 3 (che corrisponde alla lettera "g") come inizio della stringa e poi abbiamo applicato il metodo `toLowerCase()` per convertirla in minuscolo.

## Approfondimento

In Kotlin, il metodo `toLowerCase()` utilizza la regola di conversione ASCII per eseguire la conversione in minuscolo. Ciò significa che tutti i caratteri maiuscoli saranno convertiti in caratteri minuscoli, mentre i caratteri speciali e di punteggiatura rimarranno invariati.

Inoltre, il metodo `toLowerCase()` è una funzione di estensione di classe, che può essere chiamata direttamente su una stringa senza avere bisogno di utilizzare l'operatore punto ".".

## Vedi anche

- [Metodo toUpperCase() in Kotlin](https://link.to/example1)
- [Operatore di concatenazione nell'articolo di Kotlin](https://link.to/example2)