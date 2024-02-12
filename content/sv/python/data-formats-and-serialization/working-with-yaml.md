---
title:                "Att Arbeta med YAML"
date:                  2024-02-03T19:27:06.329685-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att Arbeta med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
YAML, som står för "YAML Ain't Markup Language", är ett format för serialisering av data som är läsbart för människor. Programmerare använder YAML för konfigurationsfiler, mellanprocessmeddelanden och datalagring på grund av dess enkla syntax och lättlästhet jämfört med andra format som XML eller JSON.

## Hur man gör:
Att läsa och skriva YAML i Python innebär vanligtvis användningen av ett tredjepartsbibliotek, där `PyYAML` är det mest populära. För att komma igång måste du installera PyYAML genom att köra `pip install PyYAML`.

**Exempel: Skriva till en YAML-fil**

```python
import yaml

data = {'en lista': [1, 42, 3.141, 1337, 'hjälp', u'€'],
        'en sträng': 'boo!',
        'en annan dict': {'foo': 'bar', 'nyckel': 'värde', 'svaret': 42}}

with open('example.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# Detta skapar `example.yaml` med datan strukturerad i YAML-format.
```

**Exempel: Läsa från en YAML-fil**

```python
import yaml

with open('example.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# Utdata: 
# {'en lista': [1, 42, 3.141, 1337, 'hjälp', '€'],
#  'en sträng': 'boo!',
#  'en annan dict': {'foo': 'bar', 'nyckel': 'värde', 'svaret': 42}}
```

**Använda YAML för konfiguration**

Många programmerare använder YAML för att hantera applikationskonfigurationer. Här är ett exempel på hur man kan strukturera en konfigurationsfil och läsa den:

config.yaml:
```yaml
databas:
  host: localhost
  port: 5432
  användarnamn: admin
  lösenord: hemligt
```

Läsa konfigurationsfilen i Python:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['databas']['host'])  # Utdata: localhost
```

**Hantera Komplexa Strukturer**

För komplexa strukturer tillåter PyYAML att du definierar anpassade Python-objekt. Se dock till att använda säkra metoder genom att använda `safe_load` för att undvika att utföra godtyckliga funktioner eller objekt.

```python
import yaml

# Definiera ett Python-objekt
class Exempel:
    def __init__(self, värde):
        self.värde = värde

# Anpassad konstruktor
def konstruktor_exempel(loader, node):
    värde = loader.construct_scalar(node)
    return Exempel(värde)

# Lägg till konstruktor för taggen "!exempel"
yaml.add_constructor('!exempel', konstruktor_exempel)

yaml_str = "!exempel 'data'"
loaded = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(loaded.värde)  # Utdata: data
```

I detta kodsnutt är `!exempel` en anpassad tagg som används för att instansiera ett `Exempel`-objekt med värdet 'data' från en YAML-sträng. Anpassade laddare som denna utökar flexibiliteten hos PyYAML, vilket möjliggör bearbetning av mer komplexa datastrukturer och typer.
