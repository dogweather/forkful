---
title:    "Kotlin: Unione di stringhe"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare le stringhe è una parte fondamentale della programmazione in Kotlin. Quando si lavora con stringhe, spesso è necessario combinare più stringhe per formare un'unica stringa. Questo è particolarmente utile quando si desidera creare un output personalizzato o dinamico, come un messaggio di benvenuto o un'interfaccia utente.

## Come

Per concatenare le stringhe in Kotlin, è possibile utilizzare l'operatore "+" o il metodo "plus()". Vediamo un esempio pratico:

```Kotlin
val nome = "Maria"
val cognome = "Rossi"
val nomeCompleto = nome + " " + cognome
println(nomeCompleto)
```

L'output sarà "Maria Rossi". In alternativa, possiamo utilizzare il metodo "plus()" come mostrato di seguito:

```Kotlin
val nome = "Mario"
val cognome = "Verdi"
val nomeCompleto = nome.plus(" ").plus(cognome)
println(nomeCompleto)
```

L'output sarà lo stesso di prima. È importante notare che il metodo "plus()" non modifica direttamente la stringa originale, ma ritorna una nuova stringa contenente la concatenazione delle due.

## Deep Dive

In Kotlin, le stringhe sono immutabili, il che significa che una volta create, non possono essere modificate. Quindi, quando si concatenano più stringhe, si stanno effettivamente creando nuove stringhe ogni volta. Ciò può ridurre le prestazioni del programma se eseguito molte volte. Per migliorare le prestazioni, Kotlin offre la classe "StringBuilder", che consente di modificare una stringa esistente senza crearne una nuova ogni volta.

Vediamo un esempio di utilizzo di StringBuilder per concatenare tre stringhe:

```Kotlin
val saluto = "Ciao"
val nome = "Giovanni"
val messaggio = "benvenuto/a al nostro sito"
val output = StringBuilder(saluto).append(", ").append(nome).append(", ").append(messaggio).toString()
println(output)
```

L'output sarà "Ciao, Giovanni, benvenuto/a al nostro sito". Notare come, invece di creare una nuova stringa ogni volta, si è semplicemente aggiunto il testo al StringBuilder e, alla fine, è stata convertita in una stringa utilizzando il metodo "toString()".

## Vedi anche

- Documentazione ufficiale di Kotlin: https://kotlinlang.org/docs/reference/basic-types.html#strings
- Tutorial su String Manipulation in Kotlin: https://www.programiz.com/kotlin-programming/string
- Video tutorial su concatenazione di stringhe in Kotlin: https://www.youtube.com/watch?v=IA9-xwLVcu8