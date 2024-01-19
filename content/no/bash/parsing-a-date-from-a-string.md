---
title:                "Utskrift av dato fra streng"
html_title:           "Bash: Utskrift av dato fra streng"
simple_title:         "Utskrift av dato fra streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing av dato fra en streng i Bash

## Hva & Hvorfor?

Å parse en dato fra en streng innebærer å trekke ut og behandle dato-informasjon fra en tekststreng. Det er nødvendig for å tolke og anvende datoinformasjon som blir mottatt i tekstformat.

## Slik gjør du:

Her er et enkelt bash-skript som tolker en dato fra en tekststreng.

```Bash
datoStreng="2021-12-13"
dato=$(date -d"$datoStreng" +"%d.%m.%Y")
echo $dato
```

Når du kjører dette skriptet, vil utgangen bli:

```Bash
13.12.2021
```

## Dypdykk

Bash er et skallprogrammeringsspråk med rot i Bourne Shell som ble introdusert på 1970-tallet. Til tross for sin alder, er det fortsatt hovedspråket for skripter i Linux og Unix-lignende systemer.

Det er flere måter å parse en dato fra en streng på i Bash. `date -d` kommandoen som blir brukt i eksempelet ovenfor, er sannsynligvis den enkleste og mest intuitive. Men andre verktøy som `awk`, `sed` og `cut` kan også brukes, avhengig av formateringen av inngangs strengen og personlige preferanser. 

I bunn og grunn, må date-kommandoen tolke den inngitte strengen som en dato. Dette betyr at formatet på strengen må være forståelig for datoen, noe som kan variere basert på systemdatabasenes tolkning.

## Se også

- Bash Manual: [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)
- Parse date strings in Bash: [https://stackoverflow.com/questions/3919783/parse-date-strings-in-bash](https://stackoverflow.com/questions/3919783/parse-date-strings-in-bash)
- GNU Date Manual: [https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html](https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html)

Remember, always check the versions of the programming languages and libraries you're working with. Implementations might vary.