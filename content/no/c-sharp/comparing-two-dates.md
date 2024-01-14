---
title:    "C#: Sammenligner to datoer"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer er en vanlig oppgave i programmering, spesielt når du jobber med tidspunkt og planlegging. Ved å kunne sammenligne datoer kan man enkelt sjekke om en dato kommer før eller etter en annen, eller om de er like. Dette kan være nyttig for å håndtere betingede logikker og sjekke for utløpte tidsfrister.

## Slik gjør du det

Det finnes flere måter å sammenligne datoer på i C#, men den mest effektive og nøyaktige metoden er ved å bruke `DateTime.Compare()` funksjonen. Denne funksjonen tar inn to `DateTime` objekter og returnerer en verdi basert på forholdet mellom dem. La oss se på et eksempel:

```C#
DateTime d1 = new DateTime(2020, 12, 31);
DateTime d2 = new DateTime(2021, 1, 1);

int result = DateTime.Compare(d1, d2);

if (result < 0)
{
    Console.WriteLine("Datoen " + d1.ToString("yyyy/MM/dd") + " kommer før " + d2.ToString("yyyy/MM/dd"));
}
else if (result > 0)
{
    Console.WriteLine("Datoen " + d1.ToString("yyyy/MM/dd") + " kommer etter " + d2.ToString("yyyy/MM/dd"));
}
else
{
    Console.WriteLine("Datoene er like: " + d1.ToString("yyyy/MM/dd") + " og " + d2.ToString("yyyy/MM/dd"));
}

```

I dette eksempelet oppretter vi to `DateTime` objekter og sammenligner dem ved hjelp av `DateTime.Compare()` funksjonen. Funksjonen vil returnere en verdi mindre enn 0 hvis d1 kommer før d2, en verdi større enn 0 hvis d1 kommer etter d2, og 0 hvis de er like. Vi kan deretter bruke en if/else-sjekk for å skrive ut en passende melding basert på resultatet.

## Dypere dykk

Når man sammenligner datoer, er det viktig å sørge for at man sammenligner samme aspekter av datoene. For eksempel, hvis du vil sammenligne den nøyaktige tiden på dagen, må du inkludere tidspunktet i `DateTime` objektene som sammenlignes. Hvis ikke, vil funksjonen bare sammenligne datoene på dag-nivå.

Det er også viktig å huske på at datoer kan være skrevet på forskjellige formater, noe som kan påvirke sammenligningen. Derfor er det en god praksis å bruke `ToString()` funksjonen med et spesifisert format når man skriver ut datoene.

## Se også

- [DateTime.Compare metode dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare)
- [DateTime struct dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- [Hvordan sammenligne datoer i Java](https://www.javaer101.com/no/article/1490801.html) (på engelsk)