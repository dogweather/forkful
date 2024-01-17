---
title:                "Arbeid med yaml"
html_title:           "Ruby: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
YAML er et lettvekts data serialiseringsformat som brukes av programmerere for å lagre og lese data. Det er et populært valg for konfigurasjonsfiler, internettprotokoller og andre tilfeller der data trenger å bli delt og leses av maskiner.

# Hvordan:
For å arbeide med YAML i Ruby, må du først inkludere den innebygde biblioteket `yaml` i koden din. Deretter kan du bruke metoder som `load` og `dump` for å konvertere data mellom YAML-formatet og Ruby objekter.

```Ruby
require 'yaml'

# Opprett et Ruby-objekt for å konvertere til YAML
objekt = { navn: 'Per', alder: 25 }

# Konverter Ruby-objekt til YAML-streng
yaml_streng = YAML.dump(objekt)

# Skriv YAML-streng til fil
File.write('filnavn.yml', yaml_streng)

# Les YAML-fil og konverter tilbake til Ruby-objekt
yaml_fra_fil = YAML.load_file('filnavn.yml')

# Resultat: { navn: 'Per', alder: 25 }
```

# Dypdykk:
YAML ble utviklet i 2001 av Clark Evans og Ingy döt Net. Det ble inspirert av XML, men har et enklere og mer leselig syntaks. Alternativene til YAML inkluderer JSON og XML, men YAML tilbyr et mer fleksibelt format som støtter komplekse datastrukturer som ikke er mulig i de andre formatene.

Når du konverterer mellom YAML og Ruby-objekter, vil dataene bli gjort om til en streng som representerer YAML-formatet. Dette betyr at typer og klasser som ikke støttes av YAML, som symboler og regulære uttrykk, vil bli konvertert til strenger. For å unngå dette kan du bruke spesifikke metoder som `YAML.dump_stream` for å ta vare på datastrukturene.

# Se også:
Offisiell YAML-nettside: https://yaml.org/

YAML-spesifikasjon: https://yaml.org/spec/

YAML og Ruby-dokumentasjon: https://ruby-doc.org/stdlib-2.8.0/libdoc/yaml/rdoc/YAML.html