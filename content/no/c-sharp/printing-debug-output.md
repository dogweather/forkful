---
title:                "C#: Utskrift av feilsøkingsutdata"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger, mens vi skriver koden vår, støter vi på problemer eller feil som vi ikke umiddelbart kan identifisere. Dette er når det å legge til utskrift (debug output) i koden vår kan være en nyttig løsning. Ved å skrive ut verdier og variabler mens koden kjører, kan vi få en bedre forståelse av hva som skjer, og dermed enklere finne og rette feil.

## Hvordan

For å legge til utskrift i koden vår, kan vi bruke en enkel "```Console.WriteLine()```" kommando. Her er et eksempel på hvordan dette kan se ut i C#:

```C#
string navn = "Maria";
int alder = 25;
Console.WriteLine("Navnet mitt er " + navn + " og jeg er " + alder + " år gammel.");
```

Output vil da bli:

```
Navnet mitt er Maria og jeg er 25 år gammel.
```

Vi kan også legge til variabler uten å måtte bruke string operatorer som "+" ved å bruke "```$```" tegnet og sørge for at teksten er omsluttet av "```{ }```". Her er et eksempel:

```C#
string navn = "Thomas";
int bilnummer = 123;
Console.WriteLine($"Hei, mitt navn er {navn}. Jeg har et bilnummer som er {bilnummer}.");
```

Output vil være det samme:

```
Hei, mitt navn er Thomas. Jeg har et bilnummer som er 123.
```

Vi kan også legge til utskriftsinnstillinger ved å bruke "```Console.Write()```" i stedet for "```Console.WriteLine()```". Dette vil føre til at utskriften ikke vil legge til en linjeskift, slik at vi kan skrive ut flere verdier på samme linje. Et eksempel på dette er:

```C#
int tall1 = 5;
int tall2 = 10;
Console.Write($"Tallet mitt er: {tall1} og tallet mitt er: {tall2}.");
```

Output vil bli:

```
Tallet mitt er: 5 og tallet mitt er: 10.
```

Vi kan også legge til utskrift for å vise om en bestemt del av koden vår blir utført ved å bruke "```Console.WriteLine()```" inne i "```if```" eller "```else```" uttrykk. Dette er spesielt nyttig når vi har komplekse logiske uttrykk som kan føre til forskjellige scenerioer i koden vår.

## Deep Dive

Det er mulig å legge til forskjellige typer utskrift i koden vår for å få mer informasjon og bedre forståelse av hva som skjer under kjøringen. Noen eksempler på dette er:

- Å skrive ut verdien av en bestemt variabel for å sjekke om den har riktig verdi.
- Å skrive ut en beskjed før og etter en bestemt del av koden blir utført for å se om det skjer som forventet.
- Å legge til utskrift i løkker for å se verdien av en variabel under hvert gjennomløp.
- Å bruke forskjellige farger i utskriften for å skille mellom forskjellige deler av koden.

Det er viktig å huske å fjerne utskriften når koden vår er ferdig og fungerer som den skal. Dette kan gjøres enkelt ved å kommentere ut "```Console.WriteLine()```" linjene eller ved å bruke en mer avansert løsning som "```#if DEBUG```" og "```#endif```" for å sørge for at utskriften kun vil kjøre i debug-modus.

## Se også

- [Microsoft sin offisielle dokumentasjon om Console-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.console?view=net-5.0)
- [En guide til utskrift i C#](https://www.c-sharpcorner.com/blogs/printing-output-in-c-sharp1)
- [Debugging og utskrift i Visual Studio](https://www.youtube.com/watch?v=9mh7LeMG3F4)