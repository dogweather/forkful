---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et dataformat som leser og skriver data på en måte som er lett for folk å lese og skrive. Programmerere bruker YAML for konfigurasjonsfiler og datalagring på grunn av dens klare syntaks.

## How to:
For å jobbe med YAML i Python, trenger du `PyYAML`, et bibliotek som kan installeres med pip:

```Python
pip install PyYAML
```

Last inn YAML i Python:

```Python
import yaml

# Lese YAML-string
yaml_data = """
en: Hello
no: Hei
"""

data = yaml.safe_load(yaml_data)
print(data['no'])
```

Output:

```
Hei
```

Skrive til YAML:

```Python
import yaml

data_to_write = {'en': 'Goodbye', 'no': 'Ha det'}

# Skrive data til YAML-string
yaml_data = yaml.dump(data_to_write, allow_unicode=True)
print(yaml_data)
```

Output:

```yaml
en: Goodbye
no: Ha det
```

## Deep Dive
YAML, som står for "YAML Ain't Markup Language", startet rundt 2001. Det er ment som et brukervennlig alternativ til XML for data serialisering. JSON er et annet alternativ, men YAML er ofte foretrukket for konfigurasjonsfiler der lesbarhet er viktig. Ved implementering i Python, `PyYAML` tilbyr både low-level og high-level APIer for å håndtere YAML data, med `safe_load` og `safe_dump` å anbefale for å unngå å kjøre skadelig kode gjennom YAML.

## See Also
- Offisiell YAML-nettside: [https://yaml.org/](https://yaml.org/)
- PyYAML-dokumentasjon: [https://pyyaml.org/wiki/PyYAMLDocumentation](https://pyyaml.org/wiki/PyYAMLDocumentation)
- YAML-spesifikasjon: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
