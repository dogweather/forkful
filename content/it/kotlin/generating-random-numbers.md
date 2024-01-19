---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa e perché?

Generare numeri casuali significa creare numeri che non seguono alcun modello prevedibile, ma appaiono o \ "si comportano \" in modo completamente casuale. I programmatori lo fanno per vari scopi, come simulazioni, crittografia, giochi, e persino per testare il loro codice.

## Come fare:

Generare numeri casuali in Kotlin è molto semplice. Ecco un esempio:

```Kotlin
// Crea un generatore di numeri casuali
val casual = Random
// Genera un numero intero casuale tra 0 e 100
val casualeIntero = casual.nextInt(100)
println("Intero casuale: $casualeIntero")

// Genera un numero reale casuale tra 0.0 (incluso) e 1.0 (escluso)
val casualeReale = casual.nextDouble()
println("Reale casuale: $casualeReale")
```
Stamperà qualcosa del tipo:

```
Intero casuale: 42
Reale casuale: 0.5684139419115276
```

## Approfondimento:

Generare numeri casuali è una pratica antica nel mondo della programmazione. Storicamente, i numeri casuali sono stati generati attraverso hardware fisico come i rumori atmosferici. Oggi, usiamo algoritmi matematici per la generazione pseudo-casuale dei numeri.

Oltre alla libreria `Random` di Kotlin, esistono altre librerie come `java.util.Random` e `java.security.SecureRandom` che offrono più opzioni. Tuttavia, `Random` di Kotlin offre una semplicità e una facilità d'uso che lo rende popolare.

Una cosa importante da notare: nel mondo computazionale, la causalità è più difficile di quanto sembri. I numeri generati dagli algoritmi sono tecnicamente "pseudo-casuali". Sono deterministici e prevedibili se si conosce lo stato iniziale del generatore di numeri casuali.

## Vedi anche:

Per ulteriori informazioni sulle nozioni di base della generazione di numeri casuali e sulla generazione di numeri casuali in Kotlin, consulta questi link:

- [Documentazione ufficiale di Kotlin sulla classe Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Generazione di numeri casuali su Wikipedia](https://it.wikipedia.org/wiki/Numeri_casuali)