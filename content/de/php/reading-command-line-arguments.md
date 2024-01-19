---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

---

# Lesen von Befehlszeilenargumenten in PHP

---

## Was & Warum?

Der Lesevorgang von Befehlszeilenargumenten ermöglicht den Zugriff auf Parameter, die während der Ausführung eines PHP-Scripts durch die Befehlszeile übergeben wurden. Dies ist nützlich, um skriptgesteuerte Aktionen zu ermöglichen und Inputs von Benutzern zu empfangen.

---

## So geht's:

Befehlszeilenargumente können in PHP durch Zugriff auf das globale Array `argv` und `argc` gelesen werden. Siehe unten ein einfaches Beispiel:
```PHP
<?php
// Anzahl der Argumente
echo "Anzahl der Argumente: ".$argc."\n";

// Die Argumente selbst
print_r($argv);
?>
```
Probieren wir es mit Eingabe `php script.php arg1 arg2`, die Ausgabe wäre:
```PHP
Anzahl der Argumente: 3
Array
(
    [0] => script.php
    [1] => arg1
    [2] => arg2
)
```
---

## Deep Dive:

### Historischer Kontext:

Frühere Versionen von PHP hatten diese Fähigkeit nicht direkt eingebaut. Man musste Befehlszeilenparameter über das Manipulieren von Umgebungsvariablen einlesen.

### Alternativen:

Es gibt Bibliotheken wie `getopt()` für detaillierte Befehlszeilen-Parsing oder Frameworks wie Symfony Console für erweiterte Kommandozeilen-Anwendungsfälle.

### Implementierungsdetails:

Die Variable `argc` gibt die Anzahl der Argumente wieder, einschließlich des Skriptnamens selbst. `argv` ist ein Array, das die Argumente selbst enthält, wobei `argv[0]` immer der Name des aktuellen Skripts ist.

---

## Siehe auch:

1. getopt Funktion - [PHP Dokumentation getopt()](https://www.php.net/manual/de/function.getopt.php)
2. Symfony Console Documentation - [Symfony Console](https://symfony.com/doc/current/components/console.html)
3. PHP CLI Dokumentation - [PHP: Befehlszeilen-Schnittstelle / CLI ](https://www.php.net/manual/de/features.commandline.php)

---
Bitte beachten Sie, dass die Praxis des Lesens von Befehlszeilenargumenten abhängig von den spezifischen Anforderungen Ihres Projekts variiert. Experimentieren und Lernen sind hier der Schlüssel. Viel Spaß beim Codieren!