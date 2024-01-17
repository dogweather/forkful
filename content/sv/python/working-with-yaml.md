---
title:                "Att arbeta med yaml"
html_title:           "Python: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med YAML är ett sätt för programmerare att strukturera och organisera sina data på ett mer läsbart sätt. YAML står för "YAML Ain't Markup Language" och är ett textbaserat filformat som är enkelt att läsa och skriva för människor.

## Så här gör du:
Här är ett exempel på hur man skriver en YAML-fil med Python:

```Python
import yaml
data = {
  "färg": "röd",
  "antal": 5,
  "frukter": ["äpple", "banan", "apelsin"]
}
with open("frukter.yaml", "w") as f:
  yaml.dump(data, f)
```

Detta kommer att skapa en YAML-fil med namnet "frukter.yaml" som innehåller följande data:

```yaml
färg: röd
antal: 5
frukter:
- äpple
- banan
- apelsin
```

För att läsa in data från en YAML-fil, kan man använda följande kod:

```Python
import yaml
with open("frukter.yaml") as f:
  data = yaml.safe_load(f)
print(data)
```

Detta kommer att skriva ut följande:

```yaml
{'färg': 'röd', 'antal': 5, 'frukter': ['äpple', 'banan', 'apelsin']}
```

## Djupdykning:
YAML utvecklades först 2001 av Ingy döt Net och är inspirerat av andra språk som Python, Perl och C. Det är ett populärt alternativ till XML och JSON för att strukturera data i en läsbar och lättanvänd format.

Förutom att använda YAML-filer för att spara data, kan man också använda YAML-syntax för att skriva konfigurationsfiler för program eller webbapplikationer.

Implementeringen av YAML i Python görs genom paketet "pyyaml" som kan installeras genom pip.

## Se även:
- Officiell YAML-webbplats: https://yaml.org/
- Dokumentation för pyyaml-paketet: https://pyyaml.org/wiki/PyYAMLDocumentation