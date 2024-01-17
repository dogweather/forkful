---
title:                "================================================"
html_title:           "Fish Shell: ================================================"
simple_title:         "================================================"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Arbetar du som programmerare och stöter på YAML-filer? Då är Fish Shell en bra plattform att använda sig av för att hantera och manipulera dessa filer. YAML står för "YAML Ain't Markup Language" och används för att strukturera och lagra data. Programmerare använder det ofta för konfigurationsfiler och datautbyte mellan system.

## Så här:

För att arbeta med YAML-filer i Fish Shell, kan du använda kommandot "yaml". Här är ett exempel på hur du kan lista ut alla nycklar och värden i en fil:

```
yaml data.yaml
```

Detta kommer att ge dig en lista med alla nycklar och värden i filen. Om du vill filtrera denna lista för en specifik nyckel, kan du lägga till flaggan "-k" efter kommandot:

```
yaml -k "name" data.yaml
```

Detta kommer endast att visa nycklarna och värdena för "name" i filen. Du kan också ändra värdena för en specifik nyckel genom att använda flaggan "-s" och ange det nya värdet:

```
yaml -s "age" 30 data.yaml
```

Detta kommer att ändra värdet för "age" till 30 i filen. Du kan även lägga till nya nycklar och värden med hjälp av flaggan "-a":

```
yaml -a "city" "Stockholm" data.yaml
```

Detta kommer att lägga till nyckeln "city" med värdet "Stockholm" i filen. För mer information om alla tillgängliga flaggor och hur du kan använda dem, kan du använda kommandot "yaml -h" för att få en lista med instruktioner.

## Deep Dive:

YAML grundades år 2001 av Clark Evans och blev senare en del av YAML.org. Det är en lättläst och mänskligt vänligt sätt att representera data, vilket har gjort det populärt bland programmerare. Alternativ till YAML är JSON, XML och INI-filer.

Fish Shell använder sig av ett yaml-paket för att kunna hantera YAML-filer. Detta paket är utvecklat av David R. Bild och är tillgängligt på GitHub.

## Se även:

- Fish Shell dokumentation: https://fishshell.com/docs/current/index.html
- YAML.org: https://yaml.org/
- Yaml-paket på GitHub: https://github.com/oh-my-fish/pkg-yaml