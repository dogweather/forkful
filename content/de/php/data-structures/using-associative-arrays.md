---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:09.784248-07:00
description: 'Wie geht das: In PHP ist das Erstellen und Verwenden von assoziativen
  Arrays unkompliziert. Hier eine schnelle Anleitung.'
lastmod: '2024-03-13T22:44:53.965515-06:00'
model: gpt-4-0125-preview
summary: In PHP ist das Erstellen und Verwenden von assoziativen Arrays unkompliziert.
title: Verwendung von assoziativen Arrays
weight: 15
---

## Wie geht das:
In PHP ist das Erstellen und Verwenden von assoziativen Arrays unkompliziert. Hier eine schnelle Anleitung:

```PHP
<?php
// Ein assoziatives Array erstellen
$person = array(
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
);

// Alternativ die kurze Array-Syntax
$person = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
];

// Werte mit Schlüsseln abrufen
echo "Name: " . $person["name"] . "\n";
echo "Alter: " . $person["age"] . "\n";
echo "E-Mail: " . $person["email"] . "\n";

// Einen Wert ändern
$person["age"] = 31;

// Ein neues Schlüssel-Wert-Paar hinzufügen
$person["Land"] = "USA";

// Über ein assoziatives Array iterieren
foreach ($person as $key => $value) {
    echo $key . ": " . $value . "\n";
}

// Ausgabe
// Name: John Doe
// Alter: 31
// E-Mail: john@example.com
// Land: USA
?>
```

Beachten Sie, wie Schlüssel beliebige Zeichenketten sein können, die es Ihnen erlauben, Elemente mit diesen Schlüsseln anstelle von numerischen Indizes zuzugreifen, die weniger aussagekräftig und schwerer zu merken sein können.

## Tiefergehend
Assoziative Arrays in PHP werden intern mit Hashtabellen implementiert, die einen sehr schnellen Zugriff auf Elemente mit Schlüsseln bieten und sie für viele Aufgaben hochgradig effizient machen. Diese Effizienz, kombiniert mit ihrer Benutzerfreundlichkeit, macht assoziative Arrays zu einem Grundpfeiler der PHP-Programmierung.

Historisch gesehen waren PHPs Arrays (sowohl indizierte als auch assoziative) unglaublich flexibel, was es ihnen erlaubt hat, als Listen, Stacks, Queues und mehr zu dienen. Diese Flexibilität kann jedoch manchmal zu weniger effizientem Code führen, wenn sie nicht vorsichtig verwendet wird.

In jüngerer Zeit, mit Verbesserungen in der objektorientierten Programmierung in PHP, bevorzugen einige Entwickler für strukturierte Daten, insbesondere für komplexe oder verwandte Datensätze, Objekte zu verwenden. Klassen können eine bessere Kapselung und Abstraktion bieten, den Code leichter testbar machen und Absichten klarstellen. Für einfache Schlüssel-Wert-Speicherung und unkomplizierte Datenmanipulationsszenarien bleiben assoziative Arrays jedoch aufgrund ihrer Einfachheit und der intuitiven Syntax eine ausgezeichnete Wahl.
