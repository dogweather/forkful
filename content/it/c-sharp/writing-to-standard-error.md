---
title:                "C#: Scrivere sull'errore standard"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere sulla standard error è un'azione utile per identificare e risolvere gli errori nei programm, in quanto consente di visualizzare messaggi di errore specifici e dettagliati durante l'esecuzione del codice.

## Come fare

Per scrivere sulla standard error in C#, è necessario utilizzare il metodo `Console.Error.WriteLine()`. Ecco un esempio di codice:

```C#
try 
{
    // Codice che potrebbe generare un errore
}
catch (Exception ex) 
{
    Console.Error.WriteLine("Errore: " + ex.Message);
}
```

Nell'esempio sopra, il messaggio di errore viene scritto sulla standard error utilizzando il metodo `WriteLine()` della classe `Console`, specificando il testo desiderato tra parentesi.

L'output del codice potrebbe essere qualcosa del genere:

```
Errore: Impossibile trovare il file specificato.
```

## Approfondimento

Scrivere sulla standard error è particolarmente utile quando si lavora con applicazioni console, in quanto consente di visualizzare messaggi di errore direttamente sulla console senza dover interrompere l'esecuzione del codice. Inoltre, è possibile stilizzare il testo dei messaggi di errore utilizzando sequenze di escape speciali in modo da renderli più facilmente leggibili.

Ecco un esempio di come utilizzare le sequenze di escape per impostare il colore rosso per un messaggio di errore:

```C#
Console.Error.WriteLine("\u001b[31mErrore: Impossibile trovare il file specificato.\u001b[0m");
```

L'\u001b [31m all'inizio del messaggio specifica il colore rosso, mentre l'\u001b [0m alla fine della sequenza ripristina il colore di default della console.

Utilizzando queste sequenze, è anche possibile impostare lo sfondo, lo stile del testo e altro ancora.

## Vedi anche

- [Documentazione ufficiale di Microsoft su Console.Error](https://docs.microsoft.com/it-it/dotnet/api/system.console.error)
- [Articolo su C# in Italiano](https://www.csharp-italia.it/csharp/)
- [Tutorial su sequenze di escape](https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters)