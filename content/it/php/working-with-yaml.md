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

## Cosa & Perché?
Lavorare con YAML significa gestire i dati in formato YAML usando il linguaggio di programmazione PHP. Questo formato è popolare tra i programmatori poiché è facile da leggere e scrivere per i computer, ma anche per gli esseri umani. Inoltre, YAML è altamente compatibile con una vasta gamma di linguaggi di programmazione.

## Come fare:
### Esempio 1:
```
<?php
// Creiamo un array con alcune informazioni
$frutta = array(
    "mela" => array(
        "color" => "rosso",
        "prezzo" => "1.50€"
    ),
    "banana" => array(
        "color" => "giallo",
        "prezzo" => "2€"
    )
);
// Convertiamo l'array in formato YAML
$yaml = yaml_emit($frutta);
echo $yaml;
?>
```
Output:
```
mela:
  color: rosso
  prezzo: 1.50€
banana:
  color: giallo
  prezzo: 2€
```

### Esempio 2:
```
<?php
// Creiamo una stringa con dati YAML
$car = "modello: Ferrari\nanno: 2021";
// Convertiamo la stringa in un array associativo
$array_car = yaml_parse($car);
print_r($array_car);
?>
```
Output:
```
Array ( [modello] => Ferrari [anno] => 2021 )
```

## Approfondimento:
YAML è stato creato nel 2001 da Clark Evans e Ingy döt Net come linguaggio di markup per la configurazione dei sistemi. Inizialmente era pensato per essere una versione semplificata di XML, ma è diventato rapidamente popolare tra i programmatori a causa della sua sintassi più leggibile e intuitiva. Altre alternative per la gestione dei dati includono JSON e XML.

### Dettagli Implementativi:
Per lavorare con YAML in PHP, è necessario assicurarsi che l'estensione "yaml" sia abilitata nel file php.ini. Se non è abilitata, è possibile farlo nei modi seguenti:
- Aggiungendo la linea "extension=yaml" nel file php.ini
- Usando la funzione `dl("yaml.so")` all'interno del codice PHP
- Abilitando l'estensione nell'interfaccia di gestione del server (ad esempio CPanel)

## Vedi anche:
- Documentazione ufficiale di PHP per YAML: https://www.php.net/manual/en/book.yaml.php
- Documentazione ufficiale di YAML: https://yaml.org/
- Tutorial su come lavorare con YAML in PHP: https://www.techiediaries.com/php-yaml/