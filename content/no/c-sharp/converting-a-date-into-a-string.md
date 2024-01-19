---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Konvertere en dato til en streng i C#: En veiledning

## Hva og Hvorfor?

**Hva**: Å konvertere en dato til en streng er å formidle datoen som en tekst, slik at den kan leses av mennesker eller behandles som tekstdata. **Hvorfor**: Dette tillater fleksibel utskrift, lagring og sammenligning av datoer.

## Hvordan:

Det er flere måter å utføre latekonvertering på i C#, men den vanligste metoden er å bruke `ToString`-metoden til `DateTime`-objektet. La oss se på et eksempel.

```C#
DateTime date = DateTime.Now;
string dateString = date.ToString("yyyy-MM-dd");
Console.WriteLine(dateString);
```

Utdata:

```C#
2022-04-12
```

Vi har kalt `ToString`-metoden på `date`-objektet som vi har opprettet, og gitt det et format ("yyyy-MM-dd"). Dette formatet bestemmer hvordan utdatoen vil se ut.

## Deep Dive

#### Historisk kontekst
`DateTime`-klassen, samt dens `ToString`-metode, har vært en del av .NET Framework siden dens første utgivelse i 2002. Den har tilbudt utviklere en enkel og pålitelig måte å håndtere dato- og tidsrelaterte oppgaver på.

#### Alternativer
I tillegg til `ToString`, kan en `DateTime` egenskap konverteres til en streng ved bruk av `String.Format`-metoden eller ved å bruke strenginterpolasjon.

```C#
string dateString = String.Format("{0:yyyy-MM-dd}", date);
```

Eller

```C#
string dateString = $"{date:yyyy-MM-dd}";
```

Begge er like levedyktige, men `ToString` er ofte mer strømlinjeformet.

#### Implementeringsdetaljer
Når vi snakker om implementeringsdetaljer, er det viktig å bemerke at strengen du oppretter vil avhenge av hvilket datoformat som er standard for systemet ditt. Hvis du vil sikre et bestemt format uavhengig av systeminnstillinger, bruk overbelastningen `ToString(string format, IFormatProvider provider)`, og gi den en `CultureInfo`-instans.

## Se Også

- [Microsoft: DateTime ToString (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-6.0)
- [Microsoft: Dato og tid i .NET](https://docs.microsoft.com/en-us/dotnet/standard/datetime/)
- [Stack Overflow: Konverter DateTime til en string](https://stackoverflow.com/questions/18003645/converting-datetime-to-a-string)