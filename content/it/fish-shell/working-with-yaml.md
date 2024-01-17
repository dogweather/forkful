---
title:                "Lavorare con yaml"
html_title:           "Fish Shell: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Lavorare con YAML è una pratica comune tra i programmatori, poiché è un formato di dati strutturato che permette di memorizzare informazioni in modo facilmente leggibile dalle macchine e dagli esseri umani. È comunemente utilizzato per la configurazione dei programmi e per lo scambio di dati tra sistemi.

## Come si fa:

Il modo più semplice per lavorare con YAML nel Fish Shell è utilizzando il comando `yaml`. Ad esempio, per leggere un file YAML e stamparne il contenuto, si può utilizzare il seguente comando:

```
fish-shell> yaml cat file.yaml
```

Per creare un nuovo file YAML, si può utilizzare il comando `edit` combinato con il comando `yaml`. Ad esempio:

```
fish-shell> yaml edit nuovo_file.yaml
```

Per aggiungere nuove informazioni al file YAML, si può utilizzare il comando `yaml set`. Ad esempio:

```
fish-shell> yaml set nuovo_file.yaml nome "Mario"
```

## Approfondimento

YAML è stato introdotto nel 2001 come una alternativa più facile da leggere e scrivere rispetto a formati simili come XML. È stato adottato in molti progetti open source e ha guadagnato popolarità grazie alla sua struttura intuitiva basata su liste e mappature.

Se non si utilizza Fish Shell, è comunque possibile lavorare con YAML utilizzando strumenti come Python o Ruby. In alternativa, è possibile utilizzare l'editor di testo preferito per modificare manualmente il file YAML.

Per quanto riguarda l'implementazione di YAML nel Fish Shell, il comando `yaml` fa uso della libreria di parsing PyYAML. Alcune alternative per lavorare con YAML in Fish Shell includono lo script `yed` e il plugin `fancy_yaml`.

## Vedi anche

Documentazione ufficiale di Fish Shell: https://fishshell.com/docs/current/index.html

Pagina di informazioni su YAML: https://yaml.org/