---
title:                "Utskrift av feilsøkingsutgang"
html_title:           "C#: Utskrift av feilsøkingsutgang"
simple_title:         "Utskrift av feilsøkingsutgang"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Printing debug output, eller utskrift av feilsøkingsutdata, er en vanlig praksis blant programmører. Det innebærer å skrive ut informasjon eller variabler i konsollen mens programmet kjører, for å hjelpe med feilsøking og forståelse av koden.

Programmerere gjør dette for å spore verdier av variabler, kontrollere if-setninger og løkker, og generelt forstå hvordan koden fungerer. Det kan også hjelpe med å finne kritiske feil og optimalisere kodeytelse.

## Hvordan:
```C#
Console.WriteLine("Hello World!");
//Output: Hello World!
```

```C#
int num1 = 10;
int num2 = 20;
int result = num1 + num2;
Console.WriteLine("The result of adding {0} and {1} is {2}", num1, num2, result);
//Output: The result of adding 10 and 20 is 30
```

I disse eksemplene bruker vi Console.WriteLine() -funksjonen i C# for å skrive ut tekst og variabler i konsollen. Vi kan også bruke Console.Write() for å skrive ut uten å legge til et linjeskift.

## Dykk dypere:

Printing debug output har vært en viktig del av programmering siden starten. Før moderne feilsøkingsteknikker ble utviklet, var det vanlig å bruke printlinjer for å finne feil og forstå kode.

En alternativ metode for å feilsøke er å bruke en debugger, som lar deg gå gjennom koden trinn for trinn og se verdier av variabler i sanntid. Mens debugging er en viktig del av moderne programmering, er utskrift av feilsøkingsutdata fortsatt en nyttig og enkel måte å forstå og feilsøke kode på.

Implementeringsdetaljer varierer fra språk til språk, men de grunnleggende prinsippene for printing debug output er ganske like. Det er viktig å være forsiktig når du bruker utskrift av feilsøkingsutdata i produksjonskoden, da det kan påvirke ytelsen og gjøre koden mer rotete.

## Se også:
- [Debugging with Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/?view=vs-2019)
- [Debugging with Visual Studio Code](https://code.visualstudio.com/docs/editor/debugging)