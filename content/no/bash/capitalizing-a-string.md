---
title:    "Bash: Store bokstaver i en streng"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Å øke første bokstav i en streng er en vanlig utfordring i mange programmeringsspråk. Som med de fleste oppgaver i programmering, finnes det mange forskjellige måter å løse dette på. I denne bloggposten skal vi se på hvordan å gjøre akkurat dette ved hjelp av Bash-skripting.

# Hvordan

Først må vi definere hva vi mener med å øke første bokstav i en streng. I dette tilfellet vil vi endre den første bokstaven i hvert ord til stor bokstav. For eksempel, "hei verden" skal bli "Hei Verden".

For å gjøre dette i Bash, må vi bruke innebygde funksjoner som `tr` og `sed`. Her er et eksempel på hvordan du kan gjøre dette ved hjelp av `tr`:

```
#!/bin/bash
input="hei verden"
echo "$input" | tr '[:lower:]' '[:upper:]'
```

Dette vil gi følgende utgang:

```
HEI VERDEN
```

Som du kan se, bruker vi `tr` til å konvertere alle små bokstaver til store bokstaver. Det er også mulig å bruke `sed` for å løse dette problemet. Her er en annen måte å øke første bokstav i en streng ved hjelp av `sed`:

```
#!/bin/bash
input="hei verden"
echo "$input" | sed -e "s/\b\w/\u&/g"
```

Dette vil gi følgende utgang:

```
Hei Verden
```

Her bruker vi `\u` for å konvertere den første bokstaven i hvert ord til stor bokstav.

# Deep Dive

Både `tr` og `sed` er kraftige verktøy som kan brukes til en rekke formål i Bash. Når det gjelder å øke første bokstav i en streng, er det viktig å forstå de forskjellige parametrene og mulighetene disse verktøyene har å tilby. Du kan lese mer om dem ved å bruke `man` kommandoen:

```
man tr
man sed
```

Det er også verdt å merke seg at det finnes andre måter å løse dette problemet på, for eksempel ved å bruke `awk` eller `perl`. Å utforske forskjellige måter å løse et problem på er en viktig del av å lære programmering.

# Se Også

- [Bash Manualen](https://linux.die.net/man/1/bash)
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [How to Convert String to Title Case in Bash](https://linuxhint.com/convert_string_title_case_bash/)