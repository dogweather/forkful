---
title:                "Arbeide med yaml"
html_title:           "Bash: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en programmerer som arbeider med YAML-filer, vil du sannsynligvis også ønske å kunne håndtere disse filene i Bash-skript. Dette kan være nyttig hvis du trenger å manipulere YAML-data i skriptet ditt, for eksempel når du automatiserer oppgaver eller behandler konfigurasjonsfiler.

## Slik gjør du det

Den beste måten å jobbe med YAML i Bash er å bruke verktøyet `yq`, som er en del av open-source-pakken `yq`, som lar deg behandle YAML-data fra kommandolinjen.

For å installere `yq` kan du bruke kommandoen `pip install yq` eller `brew install yq` hvis du bruker Homebrew på macOS. Hvis du ikke har `pip` eller `brew` installert, kan du finne instruksjoner på nettet for å installere dem på din enhet.

For å teste ut `yq` og se hvordan den fungerer, kan du bruke følgende YAML-fil som et eksempel:

```Bash
# example.yaml
name: John Smith
age: 30
skills:
  - programming
  - data analysis
```

For å se hele innholdet av denne filen i terminalen, kan du bruke denne kommandoen:
```Bash
yq read example.yaml
```

Dette vil gi deg følgende output:
```Bash
name: John Smith
age: 30
skills:
- programming
- data analysis
```

Du kan også hente ut spesifikke deler av YAML-filen ved å bruke `yq` til å søke etter bestemte nøkler. For eksempel, for å hente ut navnet fra YAML-filen, kan du bruke denne kommandoen:
```Bash
yq read example.yaml name
```

Dette vil gi deg følgende output:
```Bash
John Smith
```

Du kan også bruke `yq` til å endre eller legge til data i YAML-filen. For eksempel, for å legge til en ny ferdighet i `skills`-listen, kan du bruke denne kommandoen:
```Bash
yq write -i example.yaml skills[+] "project management"
```

Dette vil legge til "project management" som en ny ferdighet i YAML-filen. For å sjekke at endringen ble utført, kan du lese YAML-filen igjen ved å bruke kommandoen `yq read example.yaml`.

For flere detaljerte instruksjoner om hvordan du bruker `yq`, kan du se på dokumentasjonen på GitHub-siden deres [her](https://github.com/kislyuk/yq).

## Dypdykk

I tillegg til å bruke `yq`, kan du også jobbe med YAML i Bash ved å bruke innebygde kommandoer som `sed` eller `awk` for å manipulere YAML-data. Det er også mulig å bruke `jq`, som er et annet verktøy som ligner på `yq`, men som er laget for å håndtere JSON-filer.

Det er viktig å merke seg at `yq` og andre verktøy bare er ment for å lese og skrive til YAML-filer. Hvis du trenger å behandle store mengder YAML-data eller arbeide med komplekse YAML-strukturer, kan det være lurt å vurdere å bruke et programmeringsspråk som støtter YAML, for eksempel Python eller Ruby.

## Se også

* [yq dokumentasjon](https://github.com/kislyuk/yq)
* [YAML syntax guide](https://yaml.org/spec/1.2/spec.html)