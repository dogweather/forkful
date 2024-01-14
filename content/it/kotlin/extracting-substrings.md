---
title:    "Kotlin: Estrazione di sottostringhe"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Perché
L'estrazione di sottostringhe è un'operazione comune nella programmazione. Può essere utile per estrarre parti specifiche di una stringa o per manipolare dati in modo più efficiente.

## Come fare
Per estrarre una sottostringa in Kotlin, puoi utilizzare il metodo `substring()` su una stringa. Ad esempio, per estrarre i primi 5 caratteri da una stringa, puoi scrivere il seguente codice:

```Kotlin
val stringa = "Ciao a tutti"
val sottostringa = stringa.substring(0, 5)
println(sottostringa)
```

In questo esempio, il primo parametro indica l'indice di partenza della sottostringa e il secondo parametro indica l'indice di fine. L'esempio di codice sopra restituirebbe la sottostringa "Ciao".

È anche possibile utilizzare l'indice finale come un parametro opzionale. Se viene omesso, verranno estratti tutti i caratteri dall'indice di partenza fino alla fine della stringa. Ad esempio:

```Kotlin
val stringa = "Ciao a tutti"
val sottostringa = stringa.substring(5)
println(sottostringa)
```

Questo codice restituirebbe la sottostringa "a tutti".

## Approfondimento
Esistono anche altri metodi per estrarre sottostringhe in Kotlin, come il metodo `subSequence()` e il metodo `take()`. Inoltre, puoi anche utilizzare espressioni regolari per estrarre sottostringhe da una stringa.

Inoltre, è importante tenere presente che le stringhe in Kotlin sono immutabili, il che significa che ogni operazione di estrazione di una sottostringa creerà una nuova stringa. Se hai bisogno di manipolare un grande quantitativo di dati, è consigliabile utilizzare una classe come `StringBuilder` che consente di modificare una stringa senza crearne una nuova ogni volta.

## Vedi anche
- Documentazione di Kotlin per il metodo `substring()`: <https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html>
- Tutorial su espressioni regolari in Kotlin: <https://kotlinlang.org/docs/regexp.html>
- Documentazione di Kotlin per la classe `StringBuilder`: <https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string-builder/index.html>