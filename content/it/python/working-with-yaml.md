---
title:                "Lavorare con yaml"
html_title:           "Python: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Se hai mai lavorato con file di configurazione, è probabile che tu abbia sentito parlare di YAML. Questo linguaggio di markup leggibile dall'uomo è diventato sempre più popolare nell'ambito dello sviluppo di software per la sua semplicità e flessibilità. In questo articolo, vedremo perché dovresti considerare di utilizzare YAML nel tuo prossimo progetto.

## Come Utilizzarlo

Per utilizzare YAML in un progetto Python, è necessario prima importare il modulo `yaml`. Dopo di che, è possibile utilizzare la funzione `load()` per leggere un file YAML e convertirlo in un dizionario Python.

```Python
# Importare il modulo YAML
import yaml

# Leggere il file YAML
with open('config.yaml') as f:
    data = yaml.load(f, Loader=yaml.FullLoader)

# Stampare il risultato
print(data)
```

Supponendo che il file `config.yaml` contenga:

```YAML
nome: John Smith
età: 30 
linguaggi: 
    - Python
    - Java
    - JavaScript
```

L'output sarebbe:

```Python
{'nome': 'John Smith', 'età': 30, 'linguaggi': ['Python', 'Java', 'JavaScript']}
```

Oltre alla funzione `load()`, è possibile utilizzare anche la funzione `dump()` per convertire un dizionario Python in un file YAML.

```Python
# Dizionario Python
menu = {
    "panino": 5,
    "insalata": 7,
    "pizza": 10
}

# Convertire in YAML
print(yaml.dump(menu))
```

L'output sarebbe:

```YAML
panino: 5
insalata: 7
pizza: 10
```

## Approfondimento

Oltre alla semplice lettura e scrittura di file YAML, ci sono altre funzionalità e utilizzi che è possibile considerare. Ad esempio, è possibile combinare più file YAML utilizzando la funzione `include` per creare un file di configurazione unico per il tuo progetto. Inoltre, YAML supporta anche l'utilizzo di commenti, rendendo più leggibile il tuo codice YAML per te e per gli altri sviluppatori che lavorano sul progetto.

## Vedi Anche

- [Documentazione ufficiale di PyYAML](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [Tutorial di YAML su Real Python](https://realpython.com/python-yaml/)
- [Python YAML Configuration Files su YouTube](https://www.youtube.com/watch?v=4p0kjZYhjcQ)