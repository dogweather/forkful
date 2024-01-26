---
title:                "Arbeid med JSON"
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? 
JSON, eller JavaScript Object Notation, er et lettvekts datautvekslingsformat. Programmerere bruker det fordi det er lett å lese og skrive for mennesker og enkelt å tolke og generere for maskiner.

## How to:
Fish Shell støtter ikke direkte JSON-manipulering, men med `jq`-verktøyet blir det en lek. Installer `jq` med pakkesystemet ditt (for eksempel `apt-get install jq` på Ubuntu).

```Fish Shell
# Parse en JSON-streng og hent en verdi
echo '{"name": "Ola", "age": 28}' | jq '.name'
```
Output:
```
"Ola"
```

```Fish Shell
# Oppdater en verdi i en JSON-fil
echo '{"name": "Ola", "age": 28}' | jq '.age = 29' > updated.json
cat updated.json
```
Output:
```
{"name":"Ola","age":29}
```

## Deep Dive
JSON ble introdusert i 2001, og har siden blitt web-utviklingens språk for datautveksling. Alternativer som XML er mer verbose og tungvinte. `jq` er kraftig og kan håndtere komplekse spørringer og transformasjoner i JSON, mens lignende verktøy som `Python`-biblioteket `json` også kan være et alternativ for større applikasjoner eller skripting.

## See Also
- `jq` Manual: https://stedolan.github.io/jq/manual/
- Fish Shell Documentation: https://fishshell.com/docs/current/
- JSON Specifikasjon: https://www.json.org/json-no.html
