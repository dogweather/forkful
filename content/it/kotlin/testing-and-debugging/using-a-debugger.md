---
date: 2024-01-26 03:50:03.184815-07:00
description: "Ecco un assaggio del debugging in Kotlin con IntelliJ IDEA - lo Sherlock\
  \ Holmes degli IDE: ```kotlin fun main() { val mysteryNumber = 42 var guess = 0\u2026"
lastmod: '2024-03-13T22:44:43.395356-06:00'
model: gpt-4-0125-preview
summary: "Ecco un assaggio del debugging in Kotlin con IntelliJ IDEA - lo Sherlock\
  \ Holmes degli IDE:\n\n```kotlin\nfun main() {\n    val mysteryNumber = 42\n   \
  \ var guess = 0\n\n    while (guess ."
title: Utilizzo di un debugger
weight: 35
---

## Come fare:
Ecco un assaggio del debugging in Kotlin con IntelliJ IDEA - lo Sherlock Holmes degli IDE:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("Indovina il numero: ")
        guess = readLine()?.toIntOrNull() ?: continue // Ignora gli input non validi

        // Imposta un breakpoint qui per osservare 'guess' in azione
        if (guess < mysteryNumber) {
            println("Troppo basso!")
        } else if (guess > mysteryNumber) {
            println("Troppo alto!")
        }
    }

    println("Hai indovinato! Il numero misterioso era $mysteryNumber")
}
```

Output del debugger:
```
Indovina il numero: 
10
Troppo basso!
Indovina il numero: 
50
Troppo alto!
Indovina il numero: 
42
Hai indovinato! Il numero misterioso era 42
```

## Approfondimento
I debugger sono in gioco dagli anni '50. All'epoca erano piuttosto primitivi e il debugging poteva riguardare più l'hardware che il software. Oggi, un debugger come quello in IntelliJ IDEA ci permette di impostare breakpoint, passare attraverso il codice linea per linea e ispezionare lo stato delle variabili a nostro piacimento.

Sebbene il debugger di IntelliJ sia super utile per Kotlin, non è l'unico pesce nell'oceano. Esiste una gamma di alternative come Logcat per lo sviluppo Android, o strumenti da linea di comando come jdb per i minimalisti. La magia sottostante riguarda per lo più l'Interfaccia degli Strumenti della JVM (JVMTI), che consente ai debugger di interagire con la Java Virtual Machine, tenendo i programmatori Kotlin nel loop.

## Vedi Anche
- Documentazione del debugger di IntelliJ IDEA: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
