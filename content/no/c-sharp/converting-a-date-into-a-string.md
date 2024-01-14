---
title:    "C#: Konvertering av dato til en streng"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er en svært vanlig oppgave i programmering, spesielt i C#. Ofte trenger vi å vise dato og tid til brukeren på et leservennlig format, eller lagre datoer i en fil, database eller annen form for lagring. Å konvertere en dato til en streng gjør at vi kan endre formatet på datoen etter vårt behov og gjør informasjonen mer lesbar for brukeren.

## Slik gjør du det

Vi kan konvertere en dato til en streng ved hjelp av den innebygde metoden `.ToString()` i C#. Denne metoden tar inn et argument som bestemmer formatet på den konverterte strengen. La oss se på noen eksempler:

```C#
// Opprett en DateTime-variabel med en bestemt dato og tid
DateTime minDato = new DateTime(2021, 4, 27, 10, 30, 0);

// Konverter datoen til en streng på ulike formater
string fullDato = minDato.ToString();          // Resultat: 27.04.2021 10:30:00
string kortDato = minDato.ToString("d");        // Resultat: 27.04.2021
string langDato = minDato.ToString("D");        // Resultat: tirsdag 27. april 2021
string klokkeslett = minDato.ToString("t");     // Resultat: 10:30
string tidOgDato = minDato.ToString("g");       // Resultat: 27.04.2021 10:30
```

Som du kan se, kan vi endre formatet på datoen ved å bruke ulike argumenter i `.ToString()`-metoden. Her er noen av de vanligste argumentene som kan brukes:

- `"d"`: Kort datoformat (dd.MM.yyyy)
- `"D"`: Langt datoformat (dddd dd. MMMM yyyy)
- `"t"`: Klokkeslett (HH:mm)
- `"T"`: Klokkeslett, inkludert sekunder (HH:mm:ss)
- `"g"`: Kort dato og klokkeslett (dd.MM.yyyy HH:mm)
- `"G"`: Kort dato og klokkeslett, inkludert sekunder (dd.MM.yyyy HH:mm:ss)

Det finnes også flere andre argumenter du kan bruke for å skreddersy formatet på datoen og klokkeslettet. Det er også mulig å inkludere kultur-informasjon for å få riktig formatering basert på språket til brukeren.

## Dypdykk

Når vi kaller `.ToString()`-metoden på en dato, kalles den faktisk `.ToString()`-metoden til datoens underliggende `DateTime`-objekt. Dette objektet inneholder all informasjon om datoen, inkludert året, måneden, dagen, timen, minuttene og sekundene.

Metoden `.ToString()` har imidlertid et annet sett med argumenter når vi kaller den på `DateTime`-objektet direkte. Når vi gjør dette, kan vi inkludere formatstrenger for hver enkelt del av datoen og klokkeslettet.

For eksempel kan vi bruke følgende formatstrenger for å få individuelle deler av datoen og klokkeslettet:

- `yyyy`: År (f.eks. 2021)
- `MM`: Måned (f.eks. 04)
- `dd`: Dag (f.eks. 27)
- `HH`: Time på 24-timers format (f.eks. 10)
- `mm`: Minutter (f.eks. 30)
- `ss`: Sekunder (f.eks. 0)

Ved å kombinere disse formatstrengene kan vi få akkurat det formatet vi ønsker på datoen og klokkeslettet.

## Se også

- [DateTime.ToString() Metode (System) - Microsoft Docs](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime.tostring)
- [Custom Date and Time Format Strings - Microsoft Docs](https://docs.microsoft.com/nb-no/dotnet/standard/base-types/custom-date-and-time-format-strings)