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

## Perché lavorare con JSON

JSON (JavaScript Object Notation) è diventato uno dei formati standard per lo scambio di dati tra applicazioni web. Utilizzarlo consente di creare una comunicazione più efficiente e leggibile tra client e server.

## Come fare

Per lavorare con JSON in PHP, è necessario seguire questi semplici passaggi:

1. Utilizzare la funzione `json_encode()` per convertire un array in formato JSON.
2. Utilizzare la funzione `json_decode()` per convertire una stringa JSON in un array di PHP.
3. Utilizzare la funzione `file_get_contents()` per ottenere i dati JSON da un URL esterno.
4. Utilizzare la funzione `json_last_error()` per gestire eventuali errori durante la conversione.

Di seguito sono riportati degli esempi di codice che illustrano come utilizzare queste funzioni:

```
// Convertire un array in formato JSON
$person = array("name" => "Luca", "age" => 25);
$json = json_encode($person);

// Convertire una stringa JSON in un array di PHP
$data = '{"name": "Maria", "age": 30}';
$person = json_decode($data);

// Ottenere dati JSON da un URL esterno
$url = "https://example.com/api/people";
$json = file_get_contents($url);

// Gestire eventuali errori durante la conversione
if (json_last_error() !== JSON_ERROR_NONE) {
  // Codice per gestire l'errore
}
```

Ecco un esempio di output ottenuto dal codice sopra riportato:

```
// Output del primo esempio
{"name": "Luca", "age": 25}

// Output del secondo esempio
Array (
    [name] => Maria
    [age] => 30
)

// Output del terzo esempio (escludendo l'utilizzo di file_get_contents())
{
  "name": "Giulia",
  "age": 28
}
```

## Approfondimento

Oltre alle funzioni di base per lavorare con JSON, PHP offre anche delle utili funzioni per la manipolazione dei dati, come ad esempio:

- `json_encode()` e `json_decode()` con il parametro `JSON_PRETTY_PRINT` per ottenere una formattazione più leggibile dei dati convertiti.
- `json_encode()` con il parametro `JSON_UNESCAPED_UNICODE` per evitare l'escape dei caratteri di Unicode.
- `json_decode()` con il parametro `true` per convertire la stringa JSON in un array associativo invece di un oggetto.
- `json_last_error_msg()` per ottenere una descrizione dell'ultimo errore di conversione.

## Vedi anche

- [Documentazione PHP su JSON](https://www.php.net/manual/en/ref.json.php)
- [Introduzione a JSON](https://www.json.org/json-it.html)
- [Tutorial su come lavorare con JSON in PHP](https://www.tutorialspoint.com/php/php_working_with_json.htm)