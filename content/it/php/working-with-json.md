---
title:                "Lavorare con json"
html_title:           "PHP: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/working-with-json.md"
---

{{< edit_this_page >}}

# Cos’è e perché utilizzare JSON in PHP

## Cos’è e perché utilizzare JSON
 JSON (JavaScript Object Notation) è un formato di scrittura e trasmissione di dati utilizzato principalmente per scambiare informazioni tra un server e un client web, ma anche tra diverse applicazioni. I programmatori utilizzano JSON in PHP per gestire e manipolare in modo efficiente i dati in formato testuale.

## Come fare:
 Di seguito ti mostriamo alcuni esempi di codice PHP per lavorare con JSON. Per un utilizzo corretto, assicurati di avere installata l'ultima versione di PHP.

### Creare un oggetto JSON:
```php
$json = '{"nome": "Mario", "cognome": "Rossi", "età": 30}';
// Decodifica l'oggetto JSON in un array PHP
$dati = json_decode($json, TRUE);
```

### Aggiungere nuovi dati all'oggetto JSON:
```php
$json = '{"nome": "Mario", "cognome": "Rossi", "età": 30}';
// Decodifica l'oggetto JSON in un array PHP
$dati = json_decode($json, TRUE);
// Aggiungiamo il campo "professione" all'oggetto JSON
$dati['professione'] = "programmatore";
// Codifichiamo nuovamente l'array PHP in un oggetto JSON
$json = json_encode($dati);
```

### Stampare un oggetto JSON:
```php
$json = '{"nome": "Mario", "cognome": "Rossi", "età": 30}';
// Decodifica l'oggetto JSON in un array PHP
$dati = json_decode($json, TRUE);
// Stampiamo il campo "nome" dell'oggetto JSON
echo $dati['nome']; // Output: Mario
```

## Approfondimento:
 JSON è stato creato nel 2001 da Douglas Crockford per rendere più agevole lo scambio di dati tra applicazioni web. In passato, il formato dominante per lo scambio di dati era XML, ma JSON si è rapidamente diffuso grazie alla sua sintassi più semplice e alla maggiore leggibilità dei dati.

Oltre ad essere ampiamente utilizzato nel contesto web, JSON viene utilizzato anche in diverse applicazioni mobile e nell'Internet delle cose.

Alcune alternative a JSON includono XML, CSV e YAML. Tuttavia, JSON è considerato uno dei formati più flessibili e leggeri per lo scambio di dati tra sistemi.

Per implementare JSON in PHP, è necessario utilizzare le funzioni `json_encode()` e `json_decode()` che consentono rispettivamente di codificare e decodificare i dati in formato JSON.

## Vedi anche:
Se vuoi saperne di più su come lavorare con JSON in PHP, ti consigliamo di consultare la documentazione ufficiale di PHP su JSON: https://www.php.net/manual/en/book.json.php.

Puoi anche trovare ulteriori informazioni su JSON e il suo utilizzo sul sito ufficiale: https://www.json.org/json-it.html.

Buon coding!