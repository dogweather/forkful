---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

En strengs lengde er antall tegn. Developers finner dette ut for å håndtere data korrekt.

## Hvordan:

Her er noen eksempler på hvordan du kan finne lengden til en streng i Bash:

```Bash
# Definerer en streng
streng="Hei, Verden!"
echo ${#streng}
```

Utdataene vil være:

```Bash
14
```

Eller du kan prøve dette alternativet:

```Bash
# Alternativ måte
streng="Hei, Verden!"
len=$(echo -n $streng | wc -c)
echo $len
```
Dette vil gi samme utdata.

## Dyp dykk

Det er mange måter å finne lengden til en streng på i forskjellige programmeringsspråk. I Bash, valgte creatorne å bruke `${#variable}` syntaks for å gjøre det enkel for utviklere å jobbe med strenger. Vit at `wc -c` kommandoen kan telle bytes istedenfor tegn i noen tilfeller, så det er ikke alltid det beste valget hvis du håndterer flerbyte tegn.

## Se også

- [Bash String Manipulation](https://tldp.org/LDP/abs/html/string-manipulation.html) for mer detaljert informasjon om streng manipulering i Bash.
- [Bash Beginners Guide](https://tldp.org/LDP/Bash-Beginners-Guide/html/chap_10.html) for å lære mer om Bash og hvordan det brukes.