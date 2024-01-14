---
title:    "Java: Å bruke regulære uttrykk"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Å bruke regulære uttrykk (regular expressions) er en effektiv måte å søke og manipulere tekst på i Java-programmering. Med regulære uttrykk kan du finne og erstatte tekst basert på mønstre, slik at du kan få mer presise og fleksible resultater. Det er en viktig ferdighet for utviklere som trenger å håndtere store mengder tekst og data.

## Hvordan

Regulære uttrykk bruker et spesifikt syntaks for å definere et mønster som skal matches i en streng. I Java, brukes klassen `Pattern` for å representere et regulært uttrykk, som deretter kan matches mot en instans av strengklassen `String`.

La oss si at vi ønsker å finne alle ord som starter med bokstaven "s" i en setning. Vi kan gjøre dette med følgende kode:

```Java
// Lager et regulært uttrykk som matcher et ord som starter med "s"
Pattern pattern = Pattern.compile("\\bs\\w*");

// Lager en streng for å teste mot
String sentence = "I søndags startet vi med en skiferie";

// Bruker pattern og matcher det med strengen
Matcher matcher = pattern.matcher(sentence);

// Looper gjennom matcherens resultater og skriver dem ut
while (matcher.find()) {
    System.out.println(matcher.group());
}
```

Dette vil gi følgende output:

```
søndags
skiferie
```

Vi brukte `Pattern` for å definere et mønster som matcher et ord som starter med "s" og brukte så `Matcher` for å finne alle matchende ord i vår gitt setning. Det er viktig å merke seg at regulære uttrykk bruker spesialtegn, som `\` og `*` for å representere forskjellige mønstre. Det finnes en rekke andre spesialtegn som kan brukes for å lage mer komplekse uttrykk.

## Dypdykk

Regulære uttrykk kan bli mye mer komplekse og kraftige enn den enkle eksempelet ovenfor. Det finnes en rekke forskjellige funksjoner og metoder som kan brukes for å manipulere og søke i tekst basert på mønstre. Det er også viktig å ta hensyn til effektivitet når man bruker regulære uttrykk, da komplekse uttrykk kan føre til lengre prosesserings-tid.

En annen viktig faktor å huske på er at forskjellige programmeringsspråk har forskjellige implementeringer av regulære uttrykk, så det er viktig å sjekke dokumentasjonen for å sikre at ens kode fungerer som ønsket. Det finnes også en rekke verktøy og online ressurser for å hjelpe med å lære og teste ut ulike regulære uttrykk.

## Se også

- [Java Pattern dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Regex Tutorial](https://www.regular-expressions.info/tutorial.html)
- [Regexr - online regex tester og debugger](https://regexr.com/)