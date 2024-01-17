---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Java: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sletting av tegn som matcher et mønster er en vanlig metode innen programmering for å fjerne uønskede tegn fra en tekststreng. Dette kan være nyttig for å filtrere ut uønsket informasjon eller for å formatere en tekst på en spesifikk måte. Det er en effektiv måte å manipulere tekst på og er derfor populært blant programmere.

## Hvordan:
Det finnes ulike måter å implementere sletting av tegn basert på et mønster i Java. Et eksempel er ved å bruke metoden `replaceAll()` som tar inn et regulært uttrykk og erstatter alle tegn som matcher dette uttrykket med et annet tegn eller en tom streng. Se eksempelet nedenfor:

```Java
String tekst = "Hei, dette er en tekst med tall som jeg ønsker å fjerne: 1234567";
String nyTekst = tekst.replaceAll("\\d", "");
System.out.println(nyTekst);
```
**Output:** Hei, dette er en tekst med tall som jeg ønsker å fjerne:

Her bruker vi regulære uttrykket `\d`, som matcher alle tall i tekststrengen vår, og erstatter dem med en tom streng. Dette resulterer i at alle tallene blir fjernet fra teksten.

En annen metode er å bruke `Pattern` og `Matcher` klassene. Dette gir mer kontroll over hvilke tegn som skal fjernes og hvordan mønsteret skal utformes. Se nedenfor for et eksempel:

```Java
String tekst = "Denne teksten inneholder både tall og symboler! :) #hello";
Pattern pattern = Pattern.compile("[^a-zA-Z\\s]");
Matcher matcher = pattern.matcher(tekst);
String renTekst = matcher.replaceAll("");
System.out.println(renTekst);
```
**Output:** Denne teksten inneholder bde tall og symboler hello

Her bruker vi et regulært uttrykk som matcher alle tegn som ikke er bokstaver eller mellomrom. Disse tegnene erstattes så med en tom streng.

## Deep Dive:
Sletting av tegn basert på et mønster har vært en del av programmering i lang tid og er en viktig funksjon innen automatisering og tekstbehandling. I tillegg til Java, finnes det også alternative metoder for å håndtere tekstmanipulasjon i andre programmeringsspråk som for eksempel Python og C++. Ulike programmeringsmiljøer kan også gi ulike funksjonaliteter for å håndtere sletting av tegn. Det er derfor viktig å være bevisst på de forskjellige metodene som finnes og velge den som passer best for det spesifikke prosjektet.

Når det gjelder implementasjonsdetaljer i Java, kan det være lurt å bruke `StringBuilder` i stedet for `String` når man skal gjøre større tekstmanipulasjoner. Dette kan være mer effektivt og har mindre overhead. Det kan også være lurt å utføre slettingen i en egen tråd for mer effektivitet i større programmer.

## Se også:
- [Java Pattern og Matcher dokumentasjon](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- [Java StringBuilder dokumentasjon](https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html)