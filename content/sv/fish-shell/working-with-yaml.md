---
title:                "Arbete med YAML"
date:                  2024-01-19
simple_title:         "Arbete med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML står för "YAML Ain't Markup Language". Programmerare använder YAML för att enkelt hantera datastrukture och konfigurationsfiler, tack vare dess läslighet och enkelhet.

## Hur gör man:
Hantera YAML-filer i Fish Shell kan involvera att använda kommandon som `yq`. Här är ett exempel på hur man läser och uppdaterar YAML-innehåll:

```Fish Shell
# Installera yq med Fisherman
fisher install jorgebucaran/fisher
fisher install gazorby/fish-yq

# Läs värde från YAML-fil
yq e '.root.element' config.yaml

# Uppdatera värde och skriv tillbaka till filen
yq e '.root.element = "nytt_värde"' -i config.yaml
```

Exempelutdata när vi läser värde:
```yaml
värde_före_uppdatering
```

Efter uppdatering:
```yaml
nytt_värde
```

## Djupdykning
YAML startade runt år 2001, som ett enklare alternativ till XML. När det gäller alternativ kan programmerare välja mellan JSON, TOML eller XML. YAML skiljer sig genom sin avsaknad av klammer och användningen av indrag för att representera datahierarkier. För att hantera YAML i Fish Shell används ofta verktyg som `yq`, som är byggd ovanpå `jq` - en processor för JSON.

## Se Även
- YAML officiella hemsida: https://yaml.org
- `yq` GitHub-sida: https://github.com/mikefarah/yq
- Fish Shell-dokumentation: https://fishshell.com/docs/current/index.html
- Jämförelse av datautbytesformat: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
