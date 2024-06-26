---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:41.952221-07:00
description: "Come fare: Per l'analisi dell'HTML, i programmatori PHP possono utilizzare\
  \ funzioni integrate o affidarsi a robuste librerie come Simple HTML DOM Parser.\u2026"
lastmod: '2024-03-13T22:44:43.514405-06:00'
model: gpt-4-0125-preview
summary: Per l'analisi dell'HTML, i programmatori PHP possono utilizzare funzioni
  integrate o affidarsi a robuste librerie come Simple HTML DOM Parser.
title: Analisi del HTML
weight: 43
---

## Come fare:
Per l'analisi dell'HTML, i programmatori PHP possono utilizzare funzioni integrate o affidarsi a robuste librerie come Simple HTML DOM Parser. Qui, esploreremo esempi utilizzando sia `DOMDocument` di PHP che il Simple HTML DOM Parser.

### Utilizzando `DOMDocument`:
La classe `DOMDocument` di PHP fa parte della sua estensione DOM, consentendo di analizzare e manipolare documenti HTML e XML. Ecco un esempio rapido su come utilizzare `DOMDocument` per trovare tutte le immagini in un documento HTML:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Pagina di Esempio</title>
</head>
<body>
    <img src="image1.jpg" alt="Immagine 1">
    <img src="image2.jpg" alt="Immagine 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

Esempio di output:
```
image1.jpg
image2.jpg
```

### Utilizzando Simple HTML DOM Parser:
Per compiti più complessi o una sintassi più semplice, potresti preferire l'uso di una libreria di terze parti. Simple HTML DOM Parser è una scelta popolare, fornendo un'interfaccia simile a jQuery per navigare e manipolare le strutture HTML. Ecco come usarlo:

Prima, installa la libreria usando Composer:
```
composer require simple-html-dom/simple-html-dom
```

Poi, manipola l'HTML per, ad esempio, trovare tutti i link:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

Questo frammento di codice recupererà il contenuto HTML di 'http://www.example.com', lo analizzerà e stamperà tutti i collegamenti ipertestuali. Ricorda di sostituire `'http://www.example.com'` con l'URL effettivo che desideri analizzare.

Utilizzando questi metodi, gli sviluppatori PHP possono analizzare efficacemente i contenuti HTML, adattare l'estrazione dei dati alle loro esigenze o integrare senza problemi contenuti web esterni nei loro progetti.
