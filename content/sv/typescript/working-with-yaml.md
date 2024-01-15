---
title:                "Att arbeta med yaml"
html_title:           "TypeScript: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

För att börja arbeta med YAML, kan hjälpa dig att skapa välstrukturerade filer för att konfigurera och organisera ditt projekt. Det är också användbart för att utbyta data mellan olika plattformar.

## Hur du gör det

För att arbeta med YAML i din TypeScript-kod, måste du först installera "js-yaml" paketet. Därefter kan du använda "load" metoden för att läsa in YAML-filer och "dump" metoden för att skapa YAML-strängar. Se nedan för ett exempel på hur du läser in en YAML-fil:

```TypeScript
import * as YAML from 'js-yaml';

// Läser in YAML-fil
const data = YAML.load('exempel.yaml');

// Skriver ut innehållet till konsolen
console.log(data);
```

YAML-filer består av nyckel-värde-par, separerade med kolon. De kan också inkludera listor av värden, som visas nedan:

```yaml
namn: John Doe
ålder: 30
intressen:
  - Programmering
  - Resor
  - Musik
```

Det finns också möjlighet att använda kommentarer i YAML-filer, genom att börja raden med ett "#"-tecken. Se till att hålla koll på indentation när du skapar din YAML-fil, då det är viktigt för att få korrekt formatering.

## Deep Dive

En av fördelarna med att arbeta med YAML jämfört med andra konfigurationsfiler, är att det är mycket lättare att läsa och förstå. Det är även flexibelt och tillåter kommentarer, vilket är användbart för dokumentation. Du kan även inkludera JSON-format i din YAML-fil för att ytterligare strukturera dina data.

YAML har också inbyggd stöd för att inkludera så kallade "ankare" och "referenser", vilket gör det möjligt att återanvända samma data på flera ställen i filen. Detta är särskilt användbart för större och mer komplexa konfigurationsfiler.

## Se också

För mer information om hur du använder YAML i din TypeScript-kod, se följande länkar:

- [YAML officiell hemsida](https://yaml.org/)
- [js-yaml dokumentation](https://github.com/nodeca/js-yaml)
- [YAML Cheat Sheet](https://yaml.org/start.html)