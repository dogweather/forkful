---
title:    "Kotlin: Unendo stringhe."
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Perché
La concatenazione di stringhe è un'operazione molto comune nella programmazione, che consiste nell'unione di più stringhe per formarne una nuova. Questa pratica è utile quando si vuole creare un output più flessibile e personalizzato a seconda dei dati a disposizione.

## Come Fare
Per concatenare le stringhe in Kotlin, è possibile utilizzare l'operatore `+` o il metodo `plus()`.

```Kotlin
val str1 = "Ciao "
val str2 = "amici"

val output = str1 + str2 // Output: Ciao amici
// oppure
val output = str1.plus(str2) // Output: Ciao amici
```

Inoltre, è possibile concatenare più di due stringhe in una sola istruzione, semplicemente continuando ad aggiungere ogni stringa desiderata dopo l'operatore `+`.

```Kotlin
val output = "Buongiorno " + "a " + "tutti"  // Output: Buongiorno a tutti
```

Inoltre, è possibile concatenare anche variabili, numeri e anche espressioni all'interno della stessa istruzione.

```Kotlin
val nome = "Giulia"
val cognome = "Rossi"
val numero = 5

val output = "Ciao " + nome + " " + cognome + "! Il tuo numero fortunato è il " + numero + "."
// Output: Ciao Giulia Rossi! Il tuo numero fortunato è il 5.
```

## Approfondimento
È importante tenere in considerazione alcune cose durante la concatenazione di stringhe. In Kotlin, le stringhe sono immutabili, il che significa che non possono essere modificate una volta dichiarate. Quindi, ogni volta che viene eseguita l'operazione di concatenazione, viene creata una nuova stringa, quindi può essere una soluzione inefficiente in caso di molte operazioni.

Per risolvere questo problema, Kotlin offre la classe `StringBuilder`, che permette di modificare una stringa esistente senza crearne una nuova ogni volta.

```Kotlin
val str = java.lang.StringBuilder()

str.append("Ciao ")
str.append("amici")

println(str.toString()) // Output: Ciao amici
```

Inoltre, è possibile utilizzare la funzione di formattazione `format()` per concatenare stringhe in modo più elegante. Questa funzione permette di sostituire valori all'interno di una stringa usando il simbolo `%` seguito dal tipo di dato desiderato.

```Kotlin
val nome = "Marta"
val cognome = "Bianchi"

val output = "Ciao %s %s, come stai?".format(nome, cognome)
// Output: Ciao Marta Bianchi, come stai?
```

## Vedi Anche
- [Documentazione ufficiale di Kotlin sulla concatenazione di stringhe](https://kotlinlang.org/docs/strings.html#string-concatenation)
- [Articolo su come utilizzare la classe StringBuilder in Kotlin](https://www.baeldung.com/kotlin/stringbuilder)
- [Tutorial su come utilizzare la funzione di formattazione in Kotlin](https://dzone.com/articles/kotlin-string-formatting)