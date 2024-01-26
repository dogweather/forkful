---
title:                "Lavorare con JSON"
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con JSON significa manipolare un formato di scambio dati leggero e facilmente leggibile sia per le macchine che per gli esseri umani. I programmatori lo usano perché è uno standard de facto per le API web e per archiviare configurazioni e dati.

## How to:
```PHP
<?php
// Creare un array PHP e convertirlo in JSON
$dati = ['nome' => 'Mario', 'età' => 30, 'città' => 'Roma'];
$json = json_encode($dati);
echo $json; // Output: {"nome":"Mario","età":30,"città":"Roma"}

// Decodificare una stringa JSON in un oggetto PHP
$strJson = '{"nome":"Luigi","età":25,"città":"Milano"}';
$obj = json_decode($strJson);
echo $obj->nome; // Output: Luigi
?>
```

## Deep Dive
JSON, acronimo di JavaScript Object Notation, è nato negli anni 2000 come alternativa a XML. È diventato subito popolare per la sua semplicità e per la capacità di rappresentare dati complessi in modo compatto. Alternativamente si potrebbero utilizzare XML o YAML, ma JSON prevale per la sua compatibilità con JavaScript e per la rapidità nell'essere processato e trasferito. In PHP, `json_encode()` e `json_decode()` sono i principali metodi per la conversione tra array/oggetti e stringhe JSON.

## See Also
- [Documentazione ufficiale PHP su JSON](https://www.php.net/manual/en/book.json.php)
- [JSON.org](https://www.json.org/json-it.html) - Informazioni e specifiche su JSON
- [Tutorial PHP su W3Schools](https://www.w3schools.com/php/php_ref_json.asp)
