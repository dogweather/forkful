---
title:                "Praca z XML"
aliases:
- /pl/php/working-with-xml/
date:                  2024-01-26T04:34:14.789353-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-xml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
XML to język znaczników używany do przechowywania i transportu danych. Programiści pracują z XML, aby umożliwić interoperacyjność między aplikacjami i systemami - myśl o wymianie danych i ustawieniach konfiguracyjnych.

## Jak to zrobić:
Odczytywanie XML za pomocą SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Przypomnienie</heading>
                <body>Nie zapomnij o tym</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Wyświetla: Tove
echo $xml->from;     // Wyświetla: Jani
echo $xml->heading;  // Wyświetla: Przypomnienie
echo $xml->body;     // Wyświetla: Nie zapomnij o tym
```

Pisanie XML za pomocą DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Przypomnienie');
$body = $dom->createElement('body', 'Nie zapomnij o tym');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Przykładowy wynik:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Przypomnienie</heading>
  <body>Nie zapomnij o tym</body>
</note>
```

## Dogłębna analiza
XML czyli eXtensible Markup Language, jest podstawą serializacji danych od momentu jego rekomendacji przez W3C w 1998 roku. Jest rozwlekły, czytelny dla człowieka i ściśle określa składnię, co czyni go niezawodnym wyborem dla plików konfiguracyjnych, wymiany danych i więcej. Jednak częściowo został zacieniony przez JSON dla API internetowych ze względu na jego prostotę i lekkość.

Programiści często wybierają XML, kiedy potrzebują walidacji dokumentu zapewnianej przez Schematy XML lub gdy pracują w ekosystemach, które już mocno na nim polegają (jak formaty plików Microsoft Office). Obsługa XML w PHP jest prosta dzięki rozszerzeniu SimpleXML do podstawowych operacji. Do bardziej złożonych manipulacji DOMDocument oferuje solidny zestaw funkcji, które pozwalają na większą kontrolę, takie jak obsługa przestrzeni nazw i walidacja schematu.

## Zobacz również
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: Parsery XML w PHP](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C Schemat XML](https://www.w3.org/XML/Schema)
