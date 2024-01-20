---
title:                "Arbeid med YAML"
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et enkelt format for data serialisering, ofte brukt i konfigurasjonsfiler og datautveksling. Utviklere bruker YAML fordi det er lettleselig for mennesker og enkelt å integrere med moderne programmeringsspråk som JavaScript.

## Hvordan:
For å jobbe med YAML i JavaScript, starter vi med å parse en YAML-streng til et JavaScript-objekt. Vi ser på et eksempel med `js-yaml`-biblioteket.

```Javascript
const yaml = require('js-yaml');
const fs = require('fs');

// Les en YAML-fil og konverter til et JavaScript-objekt.
let config = yaml.load(fs.readFileSync('config.yaml', 'utf8'));

console.log(config);
```
Anta at `config.yaml` inneholder:
```yaml
versjon: 1
tjenester: 
  webapp:
    image: 'node:14'
    ports: 
      - '80:80'
```
Sample output vil være:
```Javascript
{ versjon: 1, tjenester: { webapp: { image: 'node:14', ports: ['80:80'] } } }
```

## Deep Dive
YAML koden "YAML Ain't Markup Language" (tidligere "Yet Another Markup Language") formidler essensen: enkelhet og fokus på data fremfor markup. YAML ble foreslått tidlig på 2000-tallet som et brukervennlig alternativ til XML. Mens JSON også er et alternativ for konfigurasjon og serialisering, foretrekker mange YAML for dets lesbarhet og muligheten til å kommentere koden. Når YAML konverteres til et JavaScript-objekt, behandles dataene akkurat som om de ble definert direkte i JavaScript.

## Se Også
- YAML offisiell nettside for spesifikasjoner: https://yaml.org/
- `js-yaml` GitHub-side: https://github.com/nodeca/js-yaml
- YAML vs. JSON sammenligning: https://json2yaml.com/compare-yaml-json