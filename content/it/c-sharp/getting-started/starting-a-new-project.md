---
date: 2024-01-20 18:03:14.287298-07:00
description: "Come fare: Il comando `dotnet new console` \xE8 uno standard per iniziare\
  \ un progetto console in C#. Prima del .NET 5, si usava .NET Framework o .NET Core\u2026"
lastmod: '2024-04-05T21:53:44.202704-06:00'
model: gpt-4-1106-preview
summary: "Il comando `dotnet new console` \xE8 uno standard per iniziare un progetto\
  \ console in C#."
title: Avvio di un nuovo progetto
weight: 1
---

## Come fare:
```C#
// Come creare un nuovo progetto console in C# (.NET 6+)

// 1. Apri il prompt dei comandi o la shell
// 2. Digita il seguente comando:
dotnet new console -o IlMioNuovoProgetto

// 3. Entra nella directory del progetto
cd IlMioNuovoProgetto

// 4. Ecco un semplice "Hello World" per iniziare
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Ciao Mondo!");
    }
}

// 5. Esegui il programma
dotnet run

// Dovresti vedere l'output:
// Ciao Mondo!
```

## Deep Dive
Il comando `dotnet new console` è uno standard per iniziare un progetto console in C#. Prima del .NET 5, si usava .NET Framework o .NET Core per tipi specifici di progetto. La scelta fra le varie opzioni si basava su esigenze di compatibilità e performance. .NET (ora senza 'Core' nel nome) è diventato un unico SDK per vari tipi di progetti inclusi giochi, web e desktop.

Le alternative includono la creazione di progetti con Visual Studio, un IDE che gestisce template e dipendenze, oppure l'utilizzo di Visual Studio Code con estensioni per C# per un'esperienza più leggera e flessibile.

Sapere come automatizzare questo processo è utile per l'integrazione continua (CI) e per iniziare scripting o piccoli esperimenti velocemente. Ricorda: la struttura del progetto può diventare più complessa mano a mano che cresce, quindi inizia semplice, ma pensa in grande.

## Vedi anche:
- Documentazione .NET ufficiale per iniziare: [docs.microsoft.com/dotnet/core/tutorials/with-visual-studio-code](https://docs.microsoft.com/dotnet/core/tutorials/with-visual-studio-code)
- Gestione progetti C# con Visual Studio: [docs.microsoft.com/visualstudio/ide/create-new-project](https://docs.microsoft.com/visualstudio/ide/create-new-project)
