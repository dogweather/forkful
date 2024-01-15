---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "C#: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor bry deg med å skrive ut feilsøkingsinformasjon mens du koder? Det er en rask og enkel måte å feilsøke og finne feil i koden din mens du skriver, noe som kan spare deg for mye tid og frustrasjon i det lange løp.

## Hvordan gjøre det
Det er ganske enkelt å skrive ut feilsøkingsmeldinger i C#-koden din. Alt du trenger å gjøre er å bruke "Console.WriteLine()" -funksjonen og legge til informasjonen du vil skrive ut som en parameter mellom parentesene.

```C#
Console.WriteLine("Dette er en utskrift av feilsøkingsmelding.");
```

Dette vil skrive ut teksten "Dette er en utskrift av feilsøkingsmelding" i konsollen når koden din kjører.

Du kan også inkludere variabler i utskriften ved å bruke "String.Format()" -funksjonen. For eksempel:

```C#
int num = 10;
Console.WriteLine(String.Format("Variabelen num er lik {0}", num));
```

Dette vil skrive ut "Variabelen num er lik 10" i konsollen.

Du kan også bruke "Debug.WriteLine()" -funksjonen for å skrive ut feilsøkingsmeldinger i Visual Studio Output-vinduet.

## Dykk dypere
Å skrive ut feilsøkingsmeldinger er spesielt nyttig når du jobber med store og komplekse koder. Det kan hjelpe deg med å spore feil og finne ut hvor i koden problemet ligger. Du kan også bruke denne teknikken til å logge bestemte verdier og variabler for å se hvordan de endrer seg gjennom koden din.

Det er også verdt å merke seg at når du har funnet feil og løst dem, bør du fjerne alle feilsøkingsutskrifter før du sender koden din til produksjon.

## Se også
- [Microsoft Docs - Debugging in C#](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-in-csharp?view=vs-2019)
- [GeeksforGeeks - Debugging Techniques in C#](https://www.geeksforgeeks.org/debugging-techniques-in-c-sharp/)
- [C# Corner - Debugging in Visual Studio](https://www.c-sharpcorner.com/article/debugging-in-visual-studio/)