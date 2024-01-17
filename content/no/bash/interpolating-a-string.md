---
title:                "Interpolerer en streng"
html_title:           "Bash: Interpolerer en streng"
simple_title:         "Interpolerer en streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
String interpolasjon refererer til å sette inn variabler eller verdier inn i en string. Dette er en vanlig praksis blant programmører for å lage dynamiske og mer fleksible strenger som kan tilpasses basert på ulike faktorer.

## Hvordan:
Du kan interpolere en string ved å bruke enten enkel- eller dobbel anførselstegn rundt strengen din. Deretter kan du sette inn variabler eller verdier ved hjelp av enten $-tegn eller ${}-brakettene. Se eksempler nedenfor:

```
Bash  
name="John"  
echo "Hei, mitt navn er $navn"  
```
Kjører dette vil gi oss følgende output:

```
Hei, mitt navn er John
```

For å interpolere en string med ${}-brakettene, kan du gjøre følgende:

```
Bash  
count=5  
echo "Det er ${count} epler på treet"  
```
Dette vil gi oss følgende output:

```
Det er 5 epler på treet
```

## Dypdykk:
String interpolasjon har vært en viktig del av programmering i lang tid, og er spesielt nyttig i skriptspråk som Bash. Alternativet til å interpolere en string er å bruke konkatinering, der du slår sammen separate strings. Men dette kan være mer tidkrevende og mindre fleksibelt. Når det kommer til implementasjonen, er det viktig å vite forskjellen mellom enkle og doble anførselstegn i Bash, da de har forskjellige betydninger når det kommer til string interpolasjon.

## Se også:
- [Bash dokumentasjon](https://www.gnu.org/software/bash/)
- [Stack Overflow om string interpolasjon i Bash](https://stackoverflow.com/questions/8748831/string-interpolation-in-bash)
- [En dypere forklaring på string interpolasjon](https://mikegrouchy.com/blog/2015/05/string-interpolation-in-bash.html)