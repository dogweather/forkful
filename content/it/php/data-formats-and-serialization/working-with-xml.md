---
title:                "Lavorare con XML"
date:                  2024-01-26T04:34:17.939098-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/working-with-xml.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
XML è un linguaggio di markup utilizzato per memorizzare e trasportare dati. I programmatori lavorano con XML per consentire l'interoperabilità tra applicazioni e sistemi - pensate allo scambio di dati e alle impostazioni di configurazione.

## Come fare:
Leggere XML con SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Promemoria</heading>
                <body>Non dimenticare questo</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Stampa: Tove
echo $xml->from;     // Stampa: Jani
echo $xml->heading;  // Stampa: Promemoria
echo $xml->body;     // Stampa: Non dimenticare questo
```

Scrivere XML con DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Promemoria');
$body = $dom->createElement('body', 'Non dimenticare questo');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Esempio di output:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Promemoria</heading>
  <body>Non dimenticare questo</body>
</note>
```

## Approfondimento
XML, o eXtensible Markup Language, è un pilastro nella serializzazione dei dati sin dalla sua raccomandazione W3C nel 1998. È verboso, leggibile dall'uomo e rigoroso nella sintassi, rendendolo una scelta affidabile per i file di configurazione, lo scambio di dati e altro ancora. Tuttavia, è stato parzialmente oscurato da JSON per le API web a causa della sua semplicità e leggerezza.

I programmatori spesso scelgono XML quando necessitano della validazione dei documenti fornita dagli Schemi XML o quando lavorano all'interno di ecosistemi che si affidano già pesantemente ad esso (come i formati di file di Microsoft Office). Gestire XML in PHP è semplice con l'estensione SimpleXML per le operazioni di base. Per manipolazioni più complesse, DOMDocument fornisce un robusto insieme di funzionalità che permettono un maggiore controllo, come la gestione degli spazi dei nomi e la validazione degli schemi.

## Vedi Anche
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML Schema](https://www.w3.org/XML/Schema)
