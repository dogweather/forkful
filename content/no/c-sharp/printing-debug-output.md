---
title:    "C#: Utskrift av feilrettingsutgang"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive ut feilrettingsutdata kan ofte være en nyttig måte å finne og løse problemer i koden din på. Ved å skrive ut informasjon om variabler, verdier og prosesser, kan du få en bedre forståelse av hva som skjer i koden din og hvor eventuelle feil oppstår.

## Hvordan

Du kan enkelt skrive ut feilrettingsutdata i C# ved å bruke ```Console.WriteLine()``` -funksjonen. Dette lar deg skrive ut en melding eller variabelverdi til konsollen mens koden blir kjørt. For eksempel:

```C#
string navn = "Ola";
Console.WriteLine("Hei, mitt navn er " + navn);
```

Dette vil skrive ut følgende i konsollen:

```
Hei, mitt navn er Ola
```

Du kan også bruke ```Debug.WriteLine()``` -funksjonen for å skrive ut feilrettingsutdata til Visual Studio Output-vinduet. Dette er spesielt nyttig når du jobber med større prosjekter og ønsker å se feilutdata fra forskjellige deler av koden din på ett sted. For eksempel:

```C#
int a = 5;
int b = 10;
Debug.WriteLine("a + b = " + (a + b));
```

Dette vil skrive ut følgende i Output-vinduet:

```
a + b = 15
```

Det er også mulig å bruke ```Console.WriteLine()``` og ```Debug.WriteLine()``` i kombinasjon for å skrive ut feilrettingsutdata både til konsollen og Output-vinduet.

## Dypdykk

Når du skriver ut feilrettingsutdata, er det viktig å være bevisst på hva du skriver ut og hvor du skriver ut det. For mye utdata kan gjøre det vanskelig å finne de viktige meldingene og variablene, spesielt i større prosjekter. Det kan også være lurt å bruke forskjellige utdatameldinger for å skille mellom forskjellige deler av koden din.

Det kan også være nyttig å bruke betinget feilrettingsutdata. Dette betyr å bare skrive ut informasjon når en bestemt betingelse er oppfylt. Dette er spesielt nyttig for å finne feil som bare oppstår i visse tilfeller.

## Se Også

- [MSDN documentation on debugging in C#](https://docs.microsoft.com/en-us/visualstudio/debugger/?view=vs-2019)
- [CodeCademy course on debugging in C#](https://www.codecademy.com/learn/learn-c-sharp/modules/csharp-debugging)