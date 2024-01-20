---
title:                "Sette stor bokstav i en streng"
html_title:           "C#: Sette stor bokstav i en streng"
simple_title:         "Sette stor bokstav i en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Å kapitalisere en streng i programmering betyr å endre bokstavene i strengen til store bokstaver. Programmerere gjør dette for å standardisere datainnganger eller skape et mer estetisk brukergrensesnitt.

## Hvordan:

Her er noen metoder for å kapitalisere en streng i C#:

```C#
// Bruk av ToUpper()-metode
string liten_streng = "hei der";
string stor_streng = liten_streng.ToUpper();

Console.WriteLine(stor_streng);   
// Utdata vil være: "HEI DER"

// Bruk av TextInfo.ToTitleCase() for tittelkapitalisering
System.Globalization.CultureInfo cultureInfo = System.Threading.Thread.CurrentThread.CurrentCulture;
System.Globalization.TextInfo textInfo = cultureInfo.TextInfo;

string lavtittel = "hei verden";
string stortittel = textInfo.ToTitleCase(lavtittel);

Console.WriteLine(stortittel); 
// Utdata vil være: "Hei Verden"
```

## Dyp Dykk:

Historisk sett, transformationsmetoder som ToUpper() har blitt brukt på grunn av dens enkelhet og bekvemmelighet. Imidlertid, det handler om alle bokstavene i strengen, ikke bare den første som i Title Case.

Alternativt kan du bruke TextInfo.ToTitleCase() metoden for å bare kapitalisere det første tegnet av hvert ord i en streng, det er spesielt nyttig når du arbeider med titler eller navn.

Det er viktig å understreke at disse metodene ikke endrer originale strengen. I stedet returnerer de en ny streng med de bearbeidede endringene. Det er fordi strenger i C# er uforanderlige - når de er opprettet, kan de ikke endres.

## Se også:

For mer informasjon om streng manipulasjon i C#, sjekk ut disse ressursene:

   
Husk, det er mange veier til å oppnå samme resultat i programmering, så eksperimenter og finn det som fungerer best for deg.