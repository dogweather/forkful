---
aliases:
- /it/python/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:27.005473-07:00
description: "YAML, acronimo di YAML Ain't Markup Language, \xE8 un formato di serializzazione\
  \ di dati leggibile dall'uomo. I programmatori utilizzano YAML per file di\u2026"
lastmod: 2024-02-18 23:08:55.540733
model: gpt-4-0125-preview
summary: "YAML, acronimo di YAML Ain't Markup Language, \xE8 un formato di serializzazione\
  \ di dati leggibile dall'uomo. I programmatori utilizzano YAML per file di\u2026"
title: Lavorare con YAML
---

{{< edit_this_page >}}

## Cosa & Perché?
YAML, acronimo di YAML Ain't Markup Language, è un formato di serializzazione di dati leggibile dall'uomo. I programmatori utilizzano YAML per file di configurazione, messaggistica tra processi e archiviazione di dati a causa della sua semplice sintassi e della facilità di lettura rispetto ad altri formati come XML o JSON.

## Come fare:
Leggere e scrivere YAML in Python in genere comporta l'utilizzo di una libreria di terze parti, con `PyYAML` che è la più popolare. Per iniziare, dovrai installare PyYAML eseguendo `pip install PyYAML`.

**Esempio: Scrivere su un File YAML**

```python
import yaml

data = {'una lista': [1, 42, 3.141, 1337, 'aiuto', u'€'],
        'una stringa': 'boo!',
        'un altro dizionario': {'foo': 'bar', 'chiave': 'valore', 'la risposta': 42}}

with open('esempio.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# Questo crea `esempio.yaml` con i dati strutturati in formato YAML.
```

**Esempio: Leggere da un File YAML**

```python
import yaml

with open('esempio.yaml', 'r') as f:
    dati_caricati = yaml.safe_load(f)

print(dati_caricati)

# Output: 
# {'una lista': [1, 42, 3.141, 1337, 'aiuto', '€'],
#  'una stringa': 'boo!',
#  'un altro dizionario': {'foo': 'bar', 'chiave': 'valore', 'la risposta': 42}}
```

**Uso di YAML per la Configurazione**

Molti programmatori usano YAML per gestire le configurazioni delle applicazioni. Ecco un esempio di come si potrebbe strutturare un file di configurazione e leggerlo:

config.yaml:
```yaml
database:
  host: localhost
  port: 5432
  username: admin
  password: segreto
```

Lettura del file di configurazione in Python:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['host'])  # Output: localhost
```

**Gestione di Strutture Complesse**

Per strutture complesse, PyYAML consente di definire oggetti Python personalizzati. Tuttavia, assicurati di praticare l'uso sicuro utilizzando `safe_load` per evitare l'esecuzione di funzioni o oggetti arbitrari.

```python
import yaml

# Definire un oggetto Python
class Esempio:
    def __init__(self, valore):
        self.valore = valore

# Costruttore personalizzato
def costruttore_esempio(loader, node):
    valore = loader.construct_scalar(node)
    return Esempio(valore)

# Aggiungere costruttore per il tag "!example"
yaml.add_constructor('!example', costruttore_esempio)

yaml_str = "!example 'dati'"
caricato = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(caricato.valore)  # Output: dati
```

In questo frammento, `!example` è un tag personalizzato usato per istanziare un oggetto `Esempio` con il valore 'dati' da una stringa YAML. Loader personalizzati come questo ampliano la flessibilità di PyYAML, consentendo l'elaborazione di strutture e tipi di dati più complessi.
