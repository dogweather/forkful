---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:06.329685-07:00
description: "Hur man g\xF6r: Att l\xE4sa och skriva YAML i Python inneb\xE4r vanligtvis\
  \ anv\xE4ndningen av ett tredjepartsbibliotek, d\xE4r `PyYAML` \xE4r det mest popul\xE4\
  ra. F\xF6r att\u2026"
lastmod: '2024-03-13T22:44:37.503546-06:00'
model: gpt-4-0125-preview
summary: "Att l\xE4sa och skriva YAML i Python inneb\xE4r vanligtvis anv\xE4ndningen\
  \ av ett tredjepartsbibliotek, d\xE4r `PyYAML` \xE4r det mest popul\xE4ra."
title: Att Arbeta med YAML
weight: 41
---

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
