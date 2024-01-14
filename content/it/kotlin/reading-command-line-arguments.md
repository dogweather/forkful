---
title:    "Kotlin: Lettura degli argomenti della riga di comando"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Perché

Molte volte quando si scrive un programma, può essere utile ricevere input dall'utente durante l'esecuzione del codice. Una delle forme più comuni di input è il comando da riga di comando, che permette all'utente di fornire informazioni direttamente attraverso il terminale. In questa guida, impareremo come leggere questi comandi in Kotlin.

## Come Fare

Prima di tutto, per leggere i comandi da riga di comando, è necessario aggiungere il seguente codice all'inizio del file Kotlin:

```
fun main(args: Array<String>) {
    // Il codice che legge i comandi vai qui
}
```

Questo creerà una funzione principale che accetta un array di stringhe come parametro, corrispondente ai comandi digitati dall'utente.

Per esempio, se vogliamo stampare il primo elemento del comando da riga di comando, useremo il seguente codice all'interno della funzione principale:

```
println(args[0])
```

Se l'utente digita "kotlin mioProgramma.kt argomento", questo codice stamperà "argomento", poiché è il primo elemento nell'array dei comandi.

Possiamo anche usare un ciclo for per stampare tutti gli elementi del comando da riga di comando:

```
for (arg in args) {
    println(arg)
}
```

Questo ciclo stamperà ogni elemento del comando, uno alla volta.

## Deep Dive

Oltre a stampare i comandi, possiamo anche utilizzare la funzione ```args.size``` per ottenere il numero di elementi nel comando da riga di comando. Possiamo anche convertire i comandi in tipi di dati diversi usando i metodi di conversione come ```toInt()``` e ```toDouble()```.

Inoltre, possiamo aggiungere un'opzione per leggere un input dalla riga di comando fornito dall'utente mentre il programma è in esecuzione. Utilizzando la funzione ```readLine()```, possiamo assegnare il valore inserito dall'utente a una variabile per ulteriori elaborazioni.

Con queste conoscenze, siamo pronti a utilizzare i comandi da riga di comando nei nostri programmi Kotlin!

## Vedi Anche

- [Documentazione su args in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-array/index.html)
- [Tutorial sull'uso dei comandi da riga di comando in Kotlin](https://www.jetbrains.com/help/idea/command-line-arguments.html)
- [Esempi di programmi Kotlin che utilizzano i comandi da riga di comando](https://github.com/JetBrains/kotlin-examples/tree/master/stdlib/command-line-arguments)