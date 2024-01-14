---
title:    "C#: Store bokstaver i en streng"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor kapitalisere en streng i C#

Kapitalisering av en streng er en vanlig utfordring i programmering som kan forekomme i mange forskjellige situasjoner. Dette kan inkludere å formatere en streng for presentasjon, avgjøre likhet mellom to strenger eller selv for å følge forskjellige kodestiler. Avhengig av den spesifikke bruken, kan det å kapitalisere en streng være et viktig trinn i å oppnå ønsket resultat.

## Hvordan kapitalisere en streng i C#

Det finnes flere måter å kapitalisere en streng i C# på, alt avhengig av hva du prøver å oppnå. Et enkelt eksempel kan være å bruke det innebygde String.ToUpper() metoden:

```C#
string originalStreng = "dette er en streng";
string kapitalisertStreng = originalStreng.ToUpper();

Console.WriteLine(kapitalisertStreng);
```
*Resultat: "DETTE ER EN STRENG"*

Det er viktig å merke seg at denne metoden vil kapitalisere alle bokstavene i en streng, uavhengig av om de allerede var store eller små. Hvis du bare ønsker å kapitalisere første bokstav i en streng, kan du bruke metoden String.ToUpperInvariant():

```C#
string originalStreng = "dette er en streng";
string kapitalisertStreng = char.ToUpperInvariant(originalStreng[0]) + originalStreng.Substring(1);

Console.WriteLine(kapitalisertStreng);
```
*Resultat: "Dette er en streng"*

## Dypdykk i kapitalisering av strenge

Å kapitalisere en streng i C# kan være en nyttig ferdighet å lære seg, men det er også viktig å forstå hvordan denne funksjonaliteten faktisk fungerer i koden din. Som nevnt ovenfor, kan metoden ToUpper() brukes til å konvertere en hel streng til store bokstaver. Men hvordan fungerer dette egentlig?

I likhet med mange andre programmeringsspråk, bruker C# Unicode-tegnsettet for å representere bokstaver, tall og andre symboler. Hver bokstav har en numerisk verdi som brukes for å representere den i koden. Disse verdiene brukes også til å bestemme om en bokstav er stor eller liten. Store bokstaver har en høyere numerisk verdi enn små bokstaver, noe som gjør det enkelt å konvertere fra en til den andre.

Når du bruker metoden ToUpper() på en streng, vil koden iterere gjennom hver bokstav i strengen og sjekke dens numeriske verdi. Dersom bokstavens verdi er lavere enn en viss terskelverdi, betyr det at den er en liten bokstav. I så fall vil koden øke dens numeriske verdi for å konvertere den til stor bokstav. Denne prosessen gjentas for hver bokstav i strengen, og til slutt returnerer metoden den nye strengen med alle bokstaver kapitalisert.

## Se også

Hvis du vil lære mer om å jobbe med strenger i C#, kan disse ressursene være nyttige:

- [Offisiell Microsoft C# dokumentasjon om strenger](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/strings)
- [C# Strenger Tutorial by W3Schools](https://www.w3schools.com/cs/cs_strings.asp)
- [C# String Manipulation by Codecademy](https://www.codecademy.com/learn/learn-c-sharp/modules/learn-c-sharp-strings)