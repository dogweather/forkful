---
title:                "Lavorare con yaml"
html_title:           "PHP: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché utilizzare YAML per la programmazione in PHP

Se stai lavorando con dati strutturati, come ad esempio configurazioni o informazioni da archiviare, YAML è una scelta eccellente per la tua programmazione in PHP. È un formato facile da leggere e scrivere per gli esseri umani, e possiede anche una sintassi semplice da interpretare per i computer.

## Come utilizzare YAML in PHP

Per utilizzare YAML in PHP, è necessario innanzitutto installare l'estensione "yaml" dal tuo gestore di pacchetti. Una volta installata, puoi cominciare a manipolare i dati attraverso la funzione `yaml_parse()`.

```PHP
// Esempio di parsing di un file YAML
$data = yaml_parse(file_get_contents('config.yaml'));

// Esempio di creazione di un file YAML
$config = [
  'database' => [
    'host' => 'localhost',
    'username' => 'root',
    'password' => 'password'
  ],
  'app' => [
    'name' => 'Mio fantastico sito',
    'author' => 'Io stesso'
  ]
];

file_put_contents('config.yaml', yaml_emit($config));
```

La funzione `yaml_parse()` restituisce un array PHP contenente i dati del file YAML, mentre la funzione `yaml_emit()` converte un array in formato YAML e lo scrive su un file.

## Approfondimenti su YAML

YAML è un formato di serializzazione ed è quindi utile per la memorizzazione di dati strutturati. È molto flessibile, permettendo anche l'uso di ogetti e tipi di dato personalizzati. Inoltre, YAML permette la creazione di commenti all'interno del file, rendendolo ancora più leggibile e gestibile per gli sviluppatori.

## Vedi anche

- [Documentazione ufficiale di PHP per l'estensione YAML](https://www.php.net/manual/en/book.yaml.php)
- [Documentazione ufficiale di YAML](https://yaml.org/)
- [Articolo su "Come sfruttare al meglio YAML in PHP"](https://jesseduffield.com/YAML-over-verbosity/)