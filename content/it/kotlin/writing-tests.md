---
title:    "Kotlin: Scrivere test"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test di programmazione è un modo per garantire che il nostro codice funzioni correttamente e che eventuali aggiornamenti futuri non causino problemi inaspettati. Inoltre, aiuta ad identificare e risolvere bug durante lo sviluppo.

## Come fare

Per iniziare a scrivere test in Kotlin, possiamo utilizzare il framework di testing integrato in Android Studio o Koltin. Ad esempio, possiamo creare una semplice classe di test che verifica che il metodo di somma di due numeri funzioni correttamente:

```Kotlin
// Importiamo il framework di testing
import org.junit.Test

// Definiamo la classe di test
class MyTestClass {

// Definiamo il metodo di somma
fun add(a: Int, b: Int): Int {
return a + b
}

// Definiamo il test
@Test
fun `test somma`() {
// Usiamo il metodo assertEquals per verificare il risultato della somma
assertEquals(expected = 5, actual = add(2, 3))
}
}
```

Una volta eseguito il test, dovremmo ottenere una conferma che la somma funziona correttamente.

## Approfondimento

Scrivere test può sembrare una perdita di tempo, ma in realtà ci aiuta a scrivere codice più affidabile. Inoltre, ci consente di eseguire facilmente dei test di regressione in futuro per garantire che il nostro codice continui a funzionare come previsto.

## Vedi anche

- [Documentazione ufficiale su testing in Kotlin](https://kotlinlang.org/docs/testing.html)
- [Tutorial su come scrivere test in Kotlin](https://www.youtube.com/watch?v=2TqgsVBpzuA)
- [Articolo su come il testing può migliorare la qualità del codice](https://medium.com/kotlinbrothers/the-importance-of-writing-tests-in-kotlin-44da09b8b31d)