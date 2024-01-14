---
title:    "C#: Hente nåværende dato"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være viktig å vite nøyaktig hvilken dato det er i programmet ditt. Dette kan være for å logge tidsstempler, planlegge fremtidige hendelser, eller bare for å gi informasjon til brukeren. Uansett årsak, er det viktig å vite hvordan man får tak i riktig dato i C#.

## Hvordan få tak i dagens dato i C#

For å få tak i dagens dato i C#, kan du bruke den innebygde DateTime-klassen. Denne klassen har metoder for å hente ut dato og klokkeslett, samt konvertere mellom ulike datoformater. Her er et eksempel på hvordan du kan få tak i dagens dato og skrive den ut i et bestemt format:

```C#
DateTime now = DateTime.Now;
Console.WriteLine("Dagens dato er: " + now.ToString("dd.MM.yyyy"));
```
Dette vil gi deg følgende output:

```
Dagens dato er: 08.03.2021
```

Som du kan se, bruker vi ToString()-metoden med parameteren "dd.MM.yyyy" for å formatere datoen slik vi ønsker det. Du kan også endre formatet til å inkludere måned og år, eller til og med skrive ut dagens dato og klokkeslett.

## Dypdykk

Hvis du ønsker enda mer kontroll over datoen, kan du bruke flere metoder i DateTime-klassen. For eksempel kan du få tak i kun dag, måned eller år ved å bruke Day, Month og Year-propertyene. Du kan også legge til eller trekke fra dager, timer eller andre tidsenheter ved å bruke Add- eller Subtract-metodene. Ta en titt på dokumentasjonen til DateTime-klassen for å lære om alle mulighetene.

## Se også

- [DateTime-strukturen i C#](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Omfattende guide til dato og tid i C#](https://www.c-sharpcorner.com/blogs/c-sharp-datetime-methods1)
- [Hvordan formatere dato og tid i C#](https://www.c-sharpcorner.com/UploadFile/hirendra_singh/how-to-format-date-and-time-in-C-Sharp/)