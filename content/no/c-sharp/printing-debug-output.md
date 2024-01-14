---
title:    "C#: Utskrift av feilsøkingsutdata"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Hvorfor

Å skrive ut debug output er en viktig del av å utvikle programvare. Det lar deg identifisere feil og feilsøke problemer som kan oppstå underveis i koden din. I denne bloggposten vil vi utforske hvordan du kan bruke C# for å skrive ut debug output, og hvorfor det er viktig for en effektiv utviklingsprosess.

## Hvordan

Det første du trenger å gjøre er å importere System.Diagnostics namespace i C#:

```C#
using System.Diagnostics;
```

Dette gir deg tilgang til debug-klassen, som har mange nyttige funksjoner for å skrive ut debug output. La oss se på et enkelt eksempel:

```C#
int num1 = 10;
int num2 = 5;
Debug.WriteLine("Total = " + (num1 + num2));
```

I dette eksemplet bruker vi Debug.WriteLine() for å skrive ut resultatet av en enkel matematisk operasjon. Dette vil bli skrevet ut i output-panelet i Visual Studio, og vil hjelpe oss med å verifisere at koden vår fungerer som den skal.

Det er også mulig å bruke Debug.Write() for å skrive ut en enkel verdi, eller Debug.Assert() for å sjekke om en betingelse er oppfylt og skrive ut en feilmelding hvis det er tilfelle.

## Deep Dive

Det er mange andre nyttige funksjoner i debug-klassen som kan hjelpe deg med å finne og løse problemer i koden din. For eksempel kan du bruke Debug.WriteLineIf() for å skrive ut en melding bare hvis en spesifikk betingelse er oppfylt, noe som kan være nyttig når du søker etter en spesifikk feil.

En annen nyttig funksjon er Debug.Listeners, som lar deg velge hvor debug output skal skrives. Du kan for eksempel velge å skrive ut til et loggføringsbibliotek i stedet for output-panelet i Visual Studio.

Det er også mulig å bruke conditional compilation for å sørge for at debug koden din bare blir utført i utviklingsmiljøet og ikke i produksjonsmiljøet. Dette kan bidra til å redusere unødvendig beregning og forbedre ytelsen til programvaren din.

## Se også

- [Debugging med Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-with-visual-studio)
- [Feilsøking i C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/using-the-visual-studio-debugger-for-error-handling)
- [7 tips for effektiv feilsøking i C#](https://www.c-sharpcorner.com/article/7-tips-for-effective-debugging-in-c-sharp/)