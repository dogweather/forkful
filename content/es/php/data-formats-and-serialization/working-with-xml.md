---
date: 2024-01-26 04:33:38.274788-07:00
description: "C\xF3mo hacerlo: XML, o lenguaje de marcado extensible, ha sido un pilar\
  \ en la serializaci\xF3n de datos desde su recomendaci\xF3n por el W3C en 1998.\
  \ Es\u2026"
lastmod: '2024-04-05T22:51:12.918774-06:00'
model: gpt-4-0125-preview
summary: "XML, o lenguaje de marcado extensible, ha sido un pilar en la serializaci\xF3\
  n de datos desde su recomendaci\xF3n por el W3C en 1998."
title: Trabajando con XML
weight: 40
---

## Cómo hacerlo:
Leer XML con SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Recordatorio</heading>
                <body>No olvides esto</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Devuelve: Tove
echo $xml->from;     // Devuelve: Jani
echo $xml->heading;  // Devuelve: Recordatorio
echo $xml->body;     // Devuelve: No olvides esto
```

Escribir XML con DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Recordatorio');
$body = $dom->createElement('body', 'No olvides esto');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Ejemplo de salida:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Recordatorio</heading>
  <body>No olvides esto</body>
</note>
```

## Estudio Profundo
XML, o lenguaje de marcado extensible, ha sido un pilar en la serialización de datos desde su recomendación por el W3C en 1998. Es detallado, legible por humanos y estricto en sintaxis, lo que lo convierte en una elección confiable para archivos de configuración, intercambio de datos y más. Sin embargo, ha sido parcialmente eclipsado por JSON para APIs web debido a su simplicidad y naturaleza liviana.

Los programadores a menudo eligen XML cuando necesitan validación de documentos proporcionada por los Esquemas XML o cuando trabajan dentro de ecosistemas que ya dependen en gran medida de él (como los formatos de archivo de Microsoft Office). Manejar XML en PHP es sencillo con la extensión SimpleXML para operaciones básicas. Para manipulaciones más complejas, DOMDocument ofrece un conjunto robusto de características que permiten un mayor control, como el manejo de espacios de nombres y la validación de esquemas.

## Ver también
- [PHP: SimpleXML](https://www.php.net/manual/es/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/es/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [Esquema XML del W3C](https://www.w3.org/XML/Schema)
