---
date: 2024-01-20 17:55:40.375716-07:00
description: 'How to: In C#, gli argomenti della linea di comando sono accessibili
  come array di stringhe nel metodo `Main`. Ecco un esempio.'
lastmod: '2024-03-13T22:44:43.448963-06:00'
model: gpt-4-1106-preview
summary: In C#, gli argomenti della linea di comando sono accessibili come array di
  stringhe nel metodo `Main`.
title: Lettura degli argomenti della riga di comando
weight: 23
---

## How to:
In C#, gli argomenti della linea di comando sono accessibili come array di stringhe nel metodo `Main`. Ecco un esempio:

```C#
class Program
{
    static void Main(string[] args)
    {
        foreach (var arg in args)
        {
            Console.WriteLine($"Argomento: {arg}");
        }
    }
}
```

Output di esempio se inserisci `dotnet run -- arg1 arg2 arg3`:
```
Argomento: arg1
Argomento: arg2
Argomento: arg3
```

## Deep Dive
Negli anni '80, le interfacce a riga di comando (CLI) dominavano l'interazione con i computer. L'uso degli argomenti della riga di comando è uno standard fin da allora. In C#, `Main` può usare `string[] args` o aggiornamenti recenti permettono `Main(string[] args)` anche come `async` o con parametri di tipo `Span<string>`. 

Alternative? Può usare `Environment.GetCommandLineArgs()`, che include anche il nome dell'eseguibile come primo argomento. Dettagli implementativi? Presta attenzione alla sicurezza: l'input può essere manipolato. Pulisci e valida gli argomenti prima di usarli.

## See Also
- [Microsoft Docs on Command-Line Arguments](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Overview of .NET Core CLI](https://docs.microsoft.com/en-us/dotnet/core/tools/)
