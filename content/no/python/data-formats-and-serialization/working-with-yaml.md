---
title:                "Arbeider med YAML"
date:                  2024-02-03T19:26:43.779613-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeider med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva og hvorfor?
YAML, som står for YAML Ain't Markup Language, er et menneskelesbart data serialiseringsformat. Programmerere bruker YAML for konfigurasjonsfiler, inter-prosessmeldinger og datalagring på grunn av dens enkle syntaks og lettlesthed sammenlignet med andre formater som XML eller JSON.

## Hvordan:
Å lese og skrive YAML i Python innebærer vanligvis bruk av et tredjeparts bibliotek, med `PyYAML` som det mest populære. For å komme i gang, må du installere PyYAML ved å kjøre `pip install PyYAML`.

**Eksempel: Skrive til en YAML-fil**

```python
import yaml

data = {'en liste': [1, 42, 3.141, 1337, 'hjelp', u'€'],
        'en streng': 'boo!',
        'en annen ordbok': {'foo': 'bar', 'nøkkel': 'verdi', 'svaret': 42}}

with open('eksempel.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# Dette skaper `eksempel.yaml` med data strukturert i YAML-format.
```

**Eksempel: Lese fra en YAML-fil**

```python
import yaml

with open('eksempel.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# Utdata: 
# {'en liste': [1, 42, 3.141, 1337, 'hjelp', '€'],
#  'en streng': 'boo!',
#  'en annen ordbok': {'foo': 'bar', 'nøkkel': 'verdi', 'svaret': 42}}
```

**Bruk av YAML for konfigurasjon**

Mange programmerere bruker YAML for å håndtere applikasjonskonfigurasjoner. Her er et eksempel på hvordan man kan strukturere en konfigfil og lese den:

config.yaml:
```yaml
database:
  vert: localhost
  port: 5432
  brukernavn: admin
  passord: hemmelig
```

Lese konfigurasjonsfilen i Python:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['vert'])  # Utdata: localhost
```

**Håndtering av komplekse strukturer**

For komplekse strukturer tillater PyYAML at du definerer tilpassede Python-objekter. Pass imidlertid på å bruke sikre metoder ved å benytte `safe_load` for å unngå utførelse av vilkårlige funksjoner eller objekter.

```python
import yaml

# Definer et Python-objekt
class Eksempel:
    def __init__(self, verdi):
        self.verdi = verdi

# Tilpasset konstruktør
def konstruktør_eksempel(laster, node):
    verdi = laster.construct_scalar(node)
    return Eksempel(verdi)

# Legg til konstruktør for merket "!eksempel"
yaml.add_constructor('!eksempel', konstruktør_eksempel)

yaml_str = "!eksempel 'data'"
lastet = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(lastet.verdi)  # Utdata: data
```

I dette utdraget er `!eksempel` et tilpasset merke brukt til å instansiere et `Eksempel` objekt med verdien 'data' fra en YAML-streng. Tilpassede lastere som dette utvider fleksibiliteten til PyYAML, som gjør det mulig å behandle mer komplekse datastrukturer og typer.
