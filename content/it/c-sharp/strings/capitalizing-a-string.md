---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:24.306445-07:00
description: "Come fare: C# offre un approccio diretto alla capitalizzazione delle\
  \ stringhe utilizzando metodi integrati. Il modo pi\xF9 semplice per raggiungere\
  \ questo\u2026"
lastmod: '2024-03-13T22:44:43.416865-06:00'
model: gpt-4-0125-preview
summary: C# offre un approccio diretto alla capitalizzazione delle stringhe utilizzando
  metodi integrati.
title: Capitalizzare una stringa
weight: 2
---

## Come fare:
C# offre un approccio diretto alla capitalizzazione delle stringhe utilizzando metodi integrati. Il modo più semplice per raggiungere questo risultato è modificare direttamente la stringa con questi metodi. Per regole di capitalizzazione più complesse o specifiche (ad esempio, capitalizzare ogni parola), potrebbero essere necessarie librerie aggiuntive o metodi manuali. Di seguito sono riportati esempi che dimostrano come capitalizzare una stringa in vari modi in C#.

### Capitalizzazione Base:
Per capitalizzare la prima lettera di una singola parola o frase:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // Output: "Hello world"
```

### Capitalizzare Ogni Parola:
Per capitalizzare la prima lettera di ogni parola in una stringa, puoi usare il metodo `TextInfo.ToTitleCase` trovato nello spazio dei nomi `System.Globalization`:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // Output: "Hello World"
```

Nota: `ToTitleCase` non converte in minuscolo il resto delle lettere; cambia solo in maiuscolo la prima lettera di ogni parola. Inoltre, certe parole nelle regole del titolo (come "and", "or", "of") potrebbero non essere capitalizzate a seconda delle impostazioni culturali.

### Uso di Metodi di Estensione per la Riutilizzabilità:
Puoi creare un metodo di estensione per la classe `string` per semplificare il processo di capitalizzazione, rendendo il tuo codice più pulito e più riutilizzabile. Ecco come creare e usare un tale metodo:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // Output: "Hello world"
    }
}
```

Questo metodo di estensione `Capitalize` può essere chiamato su qualsiasi oggetto stringa all'interno dello spazio dei nomi, offrendo un approccio più intuitivo e orientato agli oggetti alla manipolazione delle stringhe in C#.

### Librerie di Terze Parti:
Mentre la libreria standard di C# copre la maggior parte delle necessità per la capitalizzazione delle stringhe, certi compiti specializzati potrebbero beneficiare di librerie di terze parti, come Humanizer. Tuttavia, per il compito di semplicemente capitalizzare stringhe o ogni parola in una stringa, i metodi standard di C# sono adeguati ed efficienti, eliminando la necessità di dipendenze esterne.
