---
title:                "Å arbeide med json"
html_title:           "Fish Shell: Å arbeide med json"
simple_title:         "Å arbeide med json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

Hva og hvorfor?

Arbeider du med programmering og lurer på hva JSON er og hvorfor du bør kunne det? JSON står for JavaScript Object Notation og er et populært format for å lagre og overføre data. Det er spesielt nyttig for å overføre data mellom ulike programmeringsspråk og plattformer.

Slik gjør du det:

Fish Shell har innebygde funksjoner for å håndtere JSON-data. Du kan enkelt lese en JSON-fil ved å bruke `jq` kommandoen: 

```
jq '.key' file.json
```

Dette vil gi deg verdien til den gitte nøkkelen i filen. Du kan også velge hvilke nøkler du vil hente ut ved å bruke kommandoen: 

```
jq '.key1, .key2' file.json
```

Hvis du ønsker å endre en nøkkelverdi i en JSON-fil, kan du bruke `set` funksjonen: 

```
jq '.key |= "ny_verdi"' file.json
```

Dette vil endre verdien til den gitte nøkkelen til å være den ønskede verdien. For å lagre endringene til filen, kan du bruke `write` kommandoen: 

```
jq '.' file.json | write file.json
```

Deep Dive

JSON ble opprinnelig utviklet i 1999 og har blitt mer og mer populært de siste årene på grunn av sin enkle og fleksible struktur. Det finnes også flere alternative formater for å lagre og overføre data, for eksempel XML og YAML, men JSON er generelt sett mer leselig og enklere å jobbe med.

Fish Shell bruker et bibliotek kalt `libjq` for å håndtere JSON-data. Dette biblioteket gjør det mulig å manipulere og filtrere data basert på ulike kriterier.

Se også

For mer informasjon om JSON og hvordan du kan bruke det i din programmering, kan du se på disse kildene:

- [JSON offisiell nettside](https://json.org)
- [Fish Shell documentation](https://fishshell.com/docs/current/cmds/jq.html)
- [Tutorial om JSON i Fish Shell](https://fishshell.com/docs/current/tutorial.html#json)

Lykke til med å jobbe med JSON i Fish Shell!