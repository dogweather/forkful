---
title:                "Fish Shell: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor arbeider mange utviklere med JSON? JSON (JavaScript Object Notation) er et strukturert format som er enkelt å lese og forstå for både mennesker og datamaskiner. Det er et utmerket valg for datainterchange mellom forskjellige programmeringsspråk og plattformer.

## Hvordan det fungerer

Fish Shell gir deg en rekke kommandoer og funksjoner som gjør det enkelt å arbeide med JSON. La oss se på noen eksempler.

For å konvertere JSON-data til et Fish Shell-array, kan du bruke `from_json`-kommandoen:

```fish
set data (echo '{"navn": "Sandra", "alder": 26}' | from_json)
```

Du kan deretter hente verdiene fra dataarrayet ved å bruke `echo`-kommandoen:

```fish
echo $data[navn] # output: Sandra
```

Hvis du vil konvertere Fish Shell-arrayet tilbake til JSON, kan du bruke `to_json`-kommandoen:

```fish
echo $data | to_json # output: {"navn": "Sandra", "alder": 26}
```

## Dypdykk

Det er mange fler kommandoer og funksjoner du kan bruke i Fish Shell for å arbeide med JSON. For eksempel kan du bruke `jq`-kommandoen for å filtrere og manipulere JSON-data på en enkel måte.

Du kan også benytte deg av Fish Shells innebygde regex-funksjoner for å finne og erstatte verdier i JSON-data. Dette er spesielt nyttig hvis du ønsker å generere tilfeldig data for testing.

# Se også

- [Offisiell dokumentasjon for JSON-støtte i Fish Shell](https://fishshell.com/docs/current/cmds/from_json.html)
- [The JSON command-line tool for Fish Shell](https://github.com/fisherman/jq)
- [En tutorial for å jobbe med JSON i Fish Shell](https://dev.to/gonedark/working-with-json-in-fish-shell-465m)