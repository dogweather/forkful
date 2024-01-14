---
title:    "C#: Convertire una stringa in minuscolo"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo è una funzione fondamentale nella programmazione. Spesso può essere utile rimuovere la sensibilità alle maiuscole e minuscole per confrontare stringhe o per elaborare input dei dati in formato uniforme. In questo articolo, imparerai come eseguire questa operazione utilizzando il linguaggio di programmazione C#.

## Come fare

Per convertire una stringa in minuscolo in C#, esistono diverse opzioni. La prima è utilizzare il metodo `ToLower()` della classe `String`, che restituisce una nuova stringa in minuscolo. Ad esempio:

```C#
string input = "CIAO A TUTTI";
string output = input.ToLower();

Console.WriteLine(output);
```

L'output di questo codice sarà `ciao a tutti`. 

Un'altra opzione è utilizzare il metodo `ToLowerInvariant()`, che esegue la conversione ignorando le impostazioni culturali del sistema operativo. Questo significa che funziona allo stesso modo su qualsiasi computer, indipendentemente dalla lingua o dalle impostazioni utilizzate. Esempio:

```C#
string input = "HELLO WORLD";
string output = input.ToLowerInvariant();

Console.WriteLine(output);
```

L'output sarà ancora una volta `hello world`. 

Un'altra cosa importante da notare è che entrambi i metodi sopra descritti restituiranno una nuova stringa, piuttosto che modificare direttamente quella di input. Se si vuole fare una modifica diretta alla stringa originale, si possono utilizzare i metodi `ToLower()` e `ToLowerInvariant()` della classe `StringBuilder`. Esempio:

```C#
StringBuilder sb = new StringBuilder("CIAO MONDO");
sb.ToLower();
Console.WriteLine(sb);
```

In questo caso, l'output sarà `ciao mondo`.

## Approfondimento

Esistono alcune differenze tra i metodi `ToLower()` e `ToLowerInvariant()`, a parte il fatto che il primo opera sulla classe `String` e il secondo su `StringBuilder`. La principale di queste è che `ToLower()` può essere influenzato dalle impostazioni culturali del sistema operativo, il che significa che la conversione potrebbe essere diversa a seconda del computer sul quale viene eseguito il codice.

Ad esempio, se esegui il codice precedente su un computer con le impostazioni regionali italiane, l'output sarà `ciao mondo`. Tuttavia, se eseguito su un computer con impostazioni regionali inglesi, l'output sarà `ciao mondo`. 

D'altro canto, il metodo `ToLowerInvariant()` produrrà sempre lo stesso output, indipendentemente dall'impostazione regionale del computer.

## Vedi anche

- [Documentazione ufficiale di Microsoft su metodi di conversione delle stringhe in C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- [Esempi di utilizzo di `ToLower()` e `ToLowerInvariant()`](https://www.c-sharpcorner.com/blogs/difference-between-tolower-tostring-and-tolowerinvariant-tostring) (in inglese)