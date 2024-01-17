---
title:                "Å jobbe med json"
html_title:           "Bash: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-json.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
JSON står for JavaScript Object Notation, og det er en enkel og utbredt måte å lagre og utveksle data på. Som et programmeringsspråk, er det praktisk å arbeide med JSON fordi det er lett å lese og skrive for mennesker, og kan enkelt tolkes av maskiner.

# Hvordan:
For å arbeide med JSON i Bash, trenger du kommandolinjeverktøyet "jq". Du kan installere dette ved å kjøre ```apt-get install jq``` på Ubuntu eller ```brew install jq``` på Mac.

For å konvertere JSON til et format som er mer leselig for mennesker, kan du bruke ```jq . <json-fil>``` kommandoen. For eksempel, hvis du har en fil kalt "data.json", kan du skrive ```jq . data.json``` for å se innholdet på en mer strukturert måte.

For å hente spesifikke deler av JSON-data, kan du bruke pipes i kombinasjon med "jq" kommandoen. For eksempel, hvis du bare vil ha verdien av attributtet "name" fra JSON-dataen, kan du bruke ```jq .name``` på slutten av din "jq" kommando.

# Dypdykk:
JSON ble opprinnelig utviklet av Douglas Crockford på slutten av 1990-tallet og ble senere standardisert i ECMA-262 i 2009. Det er nå en utbredt måte å strukturere og utveksle data på nettet.

Alternativer til JSON inkluderer XML og YAML, men JSON er ofte foretrukket på grunn av sin enkelhet og lesbarhet.

"jq" er skrevet i programmet "awk" og bruker regulære uttrykk for å tolke og manipulere JSON-data.

# Se også:
For mer informasjon og dokumentasjon om "jq", sjekk ut [jq's GitHub repository](https://github.com/stedolan/jq) og [jq's official website](https://stedolan.github.io/jq/).