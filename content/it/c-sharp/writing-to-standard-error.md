---
title:                "Scrivere sull'errore standard"
html_title:           "C#: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Che cos'è e perché si usa?

Scrivere su Standard Error (stderr) è una pratica comune tra i programmatori per segnalare eventuali errori o informazioni importanti durante l'esecuzione di un programma. A differenza della scrittura su Standard Output (stdout), i dati scritti su stderr non vengono visualizzati dall'utente ma possono essere salvati in un file di log per analisi future.

## Come fare:

Utilizzare la funzione ```Console.Error.WriteLine``` per scrivere un messaggio su stderr. Ad esempio:

```C#
Console.Error.WriteLine("Errore: divisione per zero non consentita");
```

L'output sullo stderr sarà:

```
Errore: divisione per zero non consentita
```

È possibile anche salvare il contenuto di stderr su un file di log utilizzando il simbolo ```2>```. Ad esempio:

```C#
./myProgram > output.txt 2> error.log
```

In questo modo, l'output dell'esecuzione del programma sarà salvato nel file "output.txt" e gli eventuali errori o messaggi scritti su stderr saranno salvati nel file "error.log".

## Tuffo profondo:

La pratica di scrivere su stderr è nata nei primi sistemi operativi Unix, dove era comune utilizzare lo standard output per la visualizzazione dei risultati e lo standard error per la segnalazione degli errori. Nei sistemi Windows, invece, è comune utilizzare la finestra dei messaggi di errore per visualizzare eventuali problemi durante l'esecuzione di un programma.

Un'alternativa alla scrittura su stderr è l'utilizzo di log file, dove vengono salvate informazioni importanti o errori durante l'esecuzione del programma. È anche possibile utilizzare librerie di logging che offrono funzionalità più avanzate per gestire i log.

Per quanto riguarda l'implementazione, la funzione ```Console.Error.WriteLine``` utilizza una chiamata di sistema per scrivere sullo stderr del sistema operativo, quindi è importante gestire gli errori e chiudere correttamente il flusso dopo la scrittura.

## Vedi anche:

- [Documentazione ufficiale di C# su System.Console](https://docs.microsoft.com/en-us/dotnet/api/system.console?view=net-5.0)
- [Differenza tra stdout e stderr](https://unix.stackexchange.com/questions/23726/what-are-the-differences-between-stdout-and-stderr)
- [Libreria di logging NLog](https://nlog-project.org/)