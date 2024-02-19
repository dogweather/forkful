---
aliases:
- /fr/php/working-with-xml/
date: 2024-01-26 04:33:46.666094-07:00
description: "XML est un langage de balisage utilis\xE9 pour le stockage et le transport\
  \ des donn\xE9es. Les programmeurs travaillent avec XML pour permettre\u2026"
lastmod: 2024-02-18 23:09:08.949857
model: gpt-4-0125-preview
summary: "XML est un langage de balisage utilis\xE9 pour le stockage et le transport\
  \ des donn\xE9es. Les programmeurs travaillent avec XML pour permettre\u2026"
title: Travailler avec XML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
XML est un langage de balisage utilisé pour le stockage et le transport des données. Les programmeurs travaillent avec XML pour permettre l’interopérabilité entre applications et systèmes - pensez à l’échange de données et aux paramètres de configuration.

## Comment faire :
Lire du XML avec SimpleXML :

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Rappel</heading>
                <body>N'oublie pas ça</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Affiche : Tove
echo $xml->from;     // Affiche : Jani
echo $xml->heading;  // Affiche : Rappel
echo $xml->body;     // Affiche : N'oublie pas ça
```

Écrire du XML avec DOMDocument :

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Rappel');
$body = $dom->createElement('body', 'N'oublie pas ça');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Exemple de sortie :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Rappel</heading>
  <body>N'oublie pas ça</body>
</note>
```

## Approfondissement
XML, ou eXtensible Markup Language, est un pilier de la sérialisation des données depuis sa recommandation par le W3C en 1998. Il est verbeux, lisible par l'homme et strict en syntaxe, ce qui en fait un choix fiable pour les fichiers de configuration, l'échange de données, et plus encore. Cependant, il a été partiellement éclipsé par JSON pour les API web en raison de sa simplicité et de sa légèreté.

Les programmeurs choisissent souvent XML quand ils ont besoin d'une validation de documents fournie par les schémas XML ou lorsqu'ils travaillent dans des écosystèmes qui s'appuient déjà fortement sur lui (comme les formats de fichiers Microsoft Office). Manipuler XML en PHP est simple avec l'extension SimpleXML pour les opérations de base. Pour une manipulation plus complexe, DOMDocument offre un ensemble robuste de fonctionnalités qui permettent un contrôle plus grand, comme la gestion des espaces de noms et la validation des schémas.

## Voir Aussi
- [PHP : SimpleXML](https://www.php.net/manual/fr/book.simplexml.php)
- [PHP : DOMDocument](https://www.php.net/manual/fr/class.domdocument.php)
- [W3Schools : Analyseurs XML PHP](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C Schéma XML](https://www.w3.org/XML/Schema)
