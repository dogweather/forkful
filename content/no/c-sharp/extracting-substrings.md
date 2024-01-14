---
title:                "C#: Ekstrahering av substringer"
simple_title:         "Ekstrahering av substringer"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor

Mange ganger i programmering må vi jobbe med tekst og data, og det kan være nyttig å kunne hente ut deler av en tekststreng. Dette kalles å ekstrahere substrings, og det kan være svært nyttig i ulike situasjoner. I denne artikkelen vil vi se nærmere på hvordan man kan gjøre dette i C#.

# Hvordan ekstrahere substrings i C#

For å ekstrahere en substring i C#, bruker vi funksjonen `Substring()`. Denne funksjonen tar inn to parametere: startindeks og lengden på substringen du vil ekstrahere. La oss se på et praktisk eksempel:

```C#
string tekst = "Denne teksten er svært lang";
string substring = tekst.Substring(6, 6);

Console.WriteLine(substring);
```

I dette eksempelet henter vi ut en substring som starter på indeks 6 og har en lengde på 6, altså ordet "teksten". Output vil være "teksten" som forventet.

Vi kan også bruke `Substring()` til å hente ut en del av en tekststreng basert på en gitt tilstand. For eksempel, hvis vi ønsker å hente ut alt etter et bestemt tegn, kan vi bruke `IndexOf()` og `Substring()` sammen. La oss se på et eksempel:

```C#
string navn = "John Doe";
string etternavn = navn.Substring(navn.IndexOf(" ") + 1);

Console.WriteLine(etternavn);
```

Her bruker vi `IndexOf()` til å finne posisjonen til mellomrommet i teksten, og deretter bruker vi `Substring()` for å hente ut alt etter dette mellomrommet, altså "Doe".

# Dypdykk i substring-ekstrahering

Det finnes flere nyttige metoder for å ekstrahere substrings i C#, som for eksempel `Substring()` i kombinasjon med `LastIndexOf()` for å hente ut en del av tekststrengen basert på det siste forekomsten av et tegn. Det er også verdt å nevne at vi kan bruke `Substring()` sammen med både positive og negative tall for å indikere posisjonen til substringen vi ønsker å ekstrahere.

Et annet tips er å bruke `Remove()` og `RemoveAt()` for å fjerne deler av en tekststreng, og deretter bruke `Substring()` for å få tilbake den biten av teksten du ønsker å beholde.

# Se også

- Microsoft sin dokumentasjon om `Substring()` i C#: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring
- En guide til string manipulation i C#: https://www.guru99.com/c-sharp-strings.html
- Forslag til ulike metoder for å løse substring-problemer: https://www.geeksforgeeks.org/string-manipulations-in-c-sharp-set-1/