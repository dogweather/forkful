---
title:                "Å skrive en tekstfil."
html_title:           "C#: Å skrive en tekstfil."
simple_title:         "Å skrive en tekstfil."
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å skrive en tekstfil er en nyttig ferdighet å ha for enhver programmerer, uansett erfaring eller nivå. Det lar deg lagre informasjon på en enkel og organisert måte, og kan være nyttig for å lagre liten mengde data som programmet ditt trenger å lese.

## Hvordan
Skriving av en tekstfil i C# er en enkel prosess som kan gjøres med få kodelinjer. Følg disse trinnene for å opprette og skrive til en tekstfil:

1. Først må du opprette et objekt av typen `StreamWriter` for å kunne skrive til filen. Dette kan gjøres ved å inkludere `System.IO`-biblioteket og deretter skrive følgende kode:
```C#
StreamWriter fil = new StreamWriter("minfil.txt");
```
2. Nå kan du skrive innholdet du ønsker å lagre i filen. For eksempel:
```C#
fil.WriteLine("Dette er en tekstfil.");
```
3. Etter at du har skrevet alt innholdet, må du lukke filen ved å bruke `.Close()`-metoden:
```C#
fil.Close();
```
4. Filen vil nå bli opprettet og lagret på samme sted som .cs-filen din.

## Deep Dive
Å skrive en tekstfil kan også innebære å skrive til en eksisterende fil, legge til innhold i stedet for å overskrive det som allerede er der, og kontrollere tilgangsrettigheter til filen. Dette kan gjøres ved å bruke forskjellige metoder som `File.AppendText()`, `File.WriteAllLines()` og `File.SetAccessControl()`.

## Se også
- [Lesing av tekstfiler i C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0)
- [System.IO-biblioteket i C#](https://docs.microsoft.com/en-us/dotnet/api/system.io?view=net-5.0)
- [Writing Files in C#](https://www.c-sharpcorner.com/UploadFile/mahesh/writing-files-in-C-Sharp/)