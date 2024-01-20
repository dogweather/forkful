---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

# Stampare output di debug in C# - Una guida pratica

_STAMPARE OUTPUT DI DEBUG IN C#: UNA GUIDA PRATICA_.

## Cosa & Perché?
Stampare output di debug è un modo per visualizzare i valori delle variabili durante l'esecuzione del programma. Lo facciamo per capire e risolvere gli errori (bug) nel nostro codice.

## Come si fa:
Guardiamo come fare con un esempio di codice. Ecco un semplice codice di debug che utilizza la classe `Debug` in C#.

```C#
using System.Diagnostics;
public class Programma
{
    public static void Main()
    {
        Debug.WriteLine("Questo è un messaggio di debug.");
        int a = 5;
        Debug.WriteLine("Il valore di a è: " + a);
    }
}
```

L'output sarà:

```
Questo è un messaggio di debug.
Il valore di a è: 5
```

## Approfondimenti:
1. Contesto-storico: La stampa dell'output di debug è una pratica comune fin dai primi giorni della programmazione. Anche se il modo specifico di farlo varia da un linguaggio all'altro, l'idea di base è sempre la stessa.
2. Alternative: `Console.WriteLine()` è un'alternativa per la stampa di output di debug in C#. Tuttavia, a differenza di `Debug.WriteLine()`, produce output sia in modalità di debug che di rilascio.
3. Dettagli di implementazione: `Debug.WriteLine()` scrive l'output nella finestra 'Output' di Visual Studio. Puoi attivarla da VIEW > Output.

## Vedi anche:
1. [Classe Debug (Microsoft Docs)](https://docs.microsoft.com/it-it/dotnet/api/system.diagnostics.debug?view=net-5.0)