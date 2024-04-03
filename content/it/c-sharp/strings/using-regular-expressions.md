---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:29.763947-07:00
description: "Le espressioni regolari (regex) in C# sono uno strumento potente per\
  \ il riconoscimento di pattern all'interno delle stringhe, che permette ai\u2026"
lastmod: '2024-03-13T22:44:43.423939-06:00'
model: gpt-4-0125-preview
summary: Le espressioni regolari (regex) in C# sono uno strumento potente per il riconoscimento
  di pattern all'interno delle stringhe, che permette ai programmatori di cercare,
  sostituire, dividere o estrarre dati in modo efficiente.
title: Utilizzo delle espressioni regolari
weight: 11
---

## Cosa e Perché?
Le espressioni regolari (regex) in C# sono uno strumento potente per il riconoscimento di pattern all'interno delle stringhe, che permette ai programmatori di cercare, sostituire, dividere o estrarre dati in modo efficiente. I programmatori utilizzano le regex per compiti che vanno da semplici validazioni, come il controllo del formato dell'email, fino a complesse attività di elaborazione del testo, grazie alla loro flessibilità e prestazioni.

## Come fare:

### Abbinamento di Pattern Semplice
Per verificare se una stringa contiene un determinato pattern, puoi utilizzare il metodo `Regex.IsMatch` dallo spazio dei nomi `System.Text.RegularExpressions`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string testoEsempio = "Ciao, Mondo!";
        string pattern = "Mondo";
        bool contienePattern = Regex.IsMatch(testoEsempio, pattern);

        Console.WriteLine(contienePattern);  // Output: True
    }
}
```

### Estrazione dei Dati
Per estrarre dati da una stringa utilizzando gruppi in una regex si può utilizzare il metodo `Regex.Match`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string testoEsempio = "Data: 2023-04-12";
        string pattern = @"Data: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(testoEsempio, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Anno: {match.Groups[1].Value}");  // Output: Anno: 2023
            Console.WriteLine($"Mese: {match.Groups[2].Value}");  // Output: Mese: 04
            Console.WriteLine($"Giorno: {match.Groups[3].Value}");  // Output: Giorno: 12
        }
    }
}
```

### Sostituzione del Testo
Il metodo `Regex.Replace` ti permette di sostituire il testo in una stringa che corrisponde a un pattern specificato.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string testoEsempio = "Visita Microsoft!";
        string pattern = "Microsoft";
        string sostituzione = "Google";

        string risultato = Regex.Replace(testoEsempio, pattern, sostituzione);

        Console.WriteLine(risultato);  // Output: Visita Google!
    }
}
```

### Divisione delle Stringhe
Puoi dividere una stringa in un array basato su un pattern regex utilizzando il metodo `Regex.Split`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string testoEsempio = "uno,due,tre,quattro,cinque";
        string pattern = ",";

        string[] risultato = Regex.Split(testoEsempio, pattern);

        foreach (string elemento in risultato)
        {
            Console.WriteLine(elemento);
        }
        // Output: 
        // uno
        // due
        // tre
        // quattro
        // cinque
    }
}
```

### Utilizzo di Librerie di Terze Parti
Anche se il .NET Framework offre un ampio supporto per le espressioni regolari, ci sono anche librerie di terze parti come `PCRE.NET` che offrono espressioni regolari compatibili con Perl (PCRE) in C#. Questo può essere utile se hai bisogno di funzionalità o sintassi del motore regex di Perl che non sono disponibili nell'implementazione di .NET.

Per utilizzare `PCRE.NET`, dovresti prima installare il suo pacchetto NuGet, e poi puoi usarlo in modo simile a come usi le classi regex native di .NET.

```csharp
// Esempio utilizzando PCRE.NET qui
// Nota: Immagina un esempio simile a quelli sopra, adattato per mostrare una funzionalità unica di PCRE.NET.
```

Quando integri librerie di terze parti per le espressioni regolari, consulta sempre la loro documentazione per informazioni dettagliate sull'uso e sulla compatibilità.
