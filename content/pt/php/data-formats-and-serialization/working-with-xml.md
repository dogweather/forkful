---
date: 2024-01-26 04:34:01.870210-07:00
description: "Como Fazer: XML, ou Linguagem de Marca\xE7\xE3o Extens\xEDvel, tem sido\
  \ um pilar na serializa\xE7\xE3o de dados desde a sua recomenda\xE7\xE3o pela W3C\
  \ em 1998. \xC9 verboso,\u2026"
lastmod: '2024-04-05T22:50:59.941405-06:00'
model: gpt-4-0125-preview
summary: "XML, ou Linguagem de Marca\xE7\xE3o Extens\xEDvel, tem sido um pilar na\
  \ serializa\xE7\xE3o de dados desde a sua recomenda\xE7\xE3o pela W3C em 1998."
title: Trabalhando com XML
weight: 40
---

## Como Fazer:
Lendo XML com SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Lembrete</heading>
                <body>Não esqueça isso</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Saída: Tove
echo $xml->from;     // Saída: Jani
echo $xml->heading;  // Saída: Lembrete
echo $xml->body;     // Saída: Não esqueça isso
```

Escrevendo XML com DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Lembrete');
$body = $dom->createElement('body', 'Não esqueça isso');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Exemplo de Saída:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Lembrete</heading>
  <body>Não esqueça isso</body>
</note>
```

## Aprofundando
XML, ou Linguagem de Marcação Extensível, tem sido um pilar na serialização de dados desde a sua recomendação pela W3C em 1998. É verboso, legível por humanos e estrito na sintaxe, tornando-o uma escolha confiável para arquivos de configuração, troca de dados e mais. No entanto, foi parcialmente ofuscado pelo JSON para APIs web devido à sua simplicidade e natureza leve.

Programadores frequentemente escolhem XML quando precisam de validação de documentos fornecida pelos Esquemas XML ou quando trabalham em ecossistemas que já dependem muito dele (como os formatos de arquivo do Microsoft Office). Manipular XML em PHP é direto com a extensão SimpleXML para operações básicas. Para manipulações mais complexas, DOMDocument oferece um conjunto robusto de funcionalidades que permitem maior controle, como o manejo de namespaces e validação de esquemas.

## Veja Também
- [PHP: SimpleXML](https://www.php.net/manual/pt_BR/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/pt_BR/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML Schema](https://www.w3.org/XML/Schema)
