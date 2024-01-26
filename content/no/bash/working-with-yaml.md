---
title:                "Arbeid med YAML"
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML er et format for data serialisering. Programmere bruker det fordi det er lettleselig og enkelt å forstå, noe som gjør deling av data mellom mennesker og programmer grei sak.

## How to:
Lag en enkel YAML-fil `example.yml`:
```Bash
echo 'navn: Ola Nordmann
alder: 30
interesser:
  - kode
  - ski
  - musikk' > example.yml
```
Les YAML-fil i Bash med `yq` (YAML prosesseringsverktøy):
```Bash
sudo apt-get install yq
yq e '.navn' example.yml
```
Output:
```Bash
Ola Nordmann
```

## Deep Dive
YAML, som står for "YAML Ain't Markup Language", lansert i 2001, er ofte brukt i konfigurasjonsfiler og datautveksling. Alternativer som JSON eller XML finnes, men YAML vektlegger lesbarhet. Innlesning skjer vanligvis via biblioteker som `pyyaml` for Python eller `yq` for Bash.

## See Also
- YAML offisiell side: https://yaml.org
- `yq` GitHub repo: https://github.com/mikefarah/yq
- YAML tutorial: https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/
