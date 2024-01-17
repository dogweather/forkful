---
title:                "Lavorare con Yaml"
html_title:           "Gleam: Lavorare con Yaml"
simple_title:         "Lavorare con Yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

YAML è un formato di serializzazione dei dati che facilita la memorizzazione e il trasferimento di informazioni strutturate. I programmatori utilizzano YAML per leggere e scrivere configurazioni, dati di stato e altri oggetti complessi in modo semplice e leggibile.

## Come fare:

```
Gleam.export_yaml(...)
```

Gleam fornisce un modulo ```Yaml``` che espone due funzioni principali: ```export_yaml``` e ```import_yaml```. La prima prende in input un dato e lo converte in un file YAML, mentre la seconda effettua il processo inverso, leggendo un file YAML e convertendolo in un dato. Vediamo un esempio:

```
Gleam.export_yaml({ greeting: "Ciao!", numbers: [1, 2, 3]})
```

Questo genera un file YAML nel formato seguente:

```
---
greeting: Ciao!
numbers:
  - 1
  - 2
  - 3
```

## Approfondimento:

### Contesto storico:
YAML è stato sviluppato da Clark Evans nel 2001 e da allora è diventato molto popolare tra i programmatori grazie alla sua semplicità e flessibilità. È particolarmente utilizzato in ambito web per la configurazione di server e di applicazioni.

### Alternative:
Esistono diverse alternative a YAML, come ad esempio XML e JSON. Nonostante queste abbiano delle loro specifiche applicazioni, YAML è spesso preferito per la sua facilità di lettura da parte di umani e per la sua capacità di rappresentare dati complessi in modo organizzato.

### Dettagli di implementazione:
Attualmente, Gleam implementa il formato YAML versione 1.2, offrendo una precisa e completa interpretazione delle sue specifiche. È inoltre possibile specificare delle opzioni durante la conversione, come ad esempio definire il livello di indentazione o il separatore di linee.

## Vedi anche:

- [Documentazione ufficiale di YAML](https://yaml.org/)
- [Specifica della versione 1.2 di YAML](https://yaml.org/spec/1.2/spec.html)
- [Gleam Yaml module source code](https://github.com/gleam-lang/gleam_stdlib/blob/master/std/yaml/)