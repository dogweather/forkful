---
title:    "Bash: Søking og utskifting av tekst"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor: Søk og Erstatt i Bash-programmering

Søk og erstatt er en viktig funksjon i Bash-programmering, som lar deg enkelt finne og bytte ut tekst i en tekstfil eller i en variabel. Dette kan være nyttig når du jobber med store mengder data eller når du ønsker å gjøre endringer i koden din raskt og enkelt.

## Hvordan gjøre det: Eksempler og utdata

Det første trinnet for å søke og erstatte i Bash er å bruke kommandoen `sed`, som står for "stream editor". Denne kommandoen lar deg søke etter et mønster i en tekstfil og erstatte det med en annen tekst. Her er et eksempel på hvordan en `sed` kommando vil se ut i praksis:

```Bash
sed 's/apple/orange/g' file.txt
```

I dette eksempelet søker vi etter ordet "apple" i filen `file.txt` og erstatter det med "orange" overalt det blir funnet. Det siste tegnet "g" står for "global" og sørger for at alle forekomster av mønsteret blir erstattet. Her er et eksempel på en fil før og etter bruk av `sed` kommandoen:

Før:
```
Apple is delicious.
I love eating apples.
Apple pie is my favorite dessert.
```

Etter:
```
orange is delicious.
I love eating oranges.
orange pie is my favorite dessert.
```

Du kan også bruke `sed` kommandoen til å søke og erstatte i en variabel i Bash. Her er et eksempel på hvordan dette kan gjøres:

```Bash
fruit="apple, banana, cherry"

echo $fruit | sed 's/apple/orange/g'
```

Dette vil gi følgende utdata:
```
orange, banana, cherry
```

## Dykk dypere: Mer informasjon om søk og erstat

For mer avanserte søk og erstatt operasjoner, kan du bruke regex (regular expressions) i `sed` kommandoen. Dette lar deg søke etter komplekse mønstre og utføre avanserte erstatninger. Her er et eksempel på hvordan dette kan gjøres:

```Bash
sed 's/[0-9]/X/g' file.txt
```

I dette eksempelet søker vi etter alle tall (representert av [0-9]) i filen `file.txt` og erstatter dem med bokstaven X. Resultatet vil da bli som følger:

```
Apple is delicious.
I love eating apples.
Apple pie is my favorite dessert.
```

Etter:
```
XXXXX XX XXXXXXXXX.
I love eating apples.
Apple pie is my favorite dessert.
```

Du kan også bruke andre kommandoer som `awk` og `perl` for å gjøre lignende søk og erstatt operasjoner. Det er også verdt å utforske forskjellige flagg og alternativer som kan brukes med `sed` kommandoen for å oppnå ønsket resultat.

## Se også

- [Bash sed kommandoen dokumentasjon](https://www.gnu.org/software/sed/manual/sed.html)
- [Bash scripting tutorial](https://www.tutorialspoint.com/unix/shell_scripting.htm)
- [RegEx tutorial](https://www.regular-expressions.info/)