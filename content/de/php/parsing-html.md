---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTML-Parsing ist der Prozess, bei dem wir ein HTML-Dokument analysieren und strukturieren. Programmierer machen das, weil sie dadurch Daten extrahieren oder manipulieren können - oftmals automatisiert.

## So geht's:
Nun zeigen wir, wie das geht, indem wir die DOMDocument-Klasse in PHP nutzen:

```PHP
<?php
// Erzeuge ein neues Dokument
$doc = new DOMDocument();

// Lade dein HTML
$doc->loadHTML('
<html>
    <body>
        <h1>Willkommen auf meiner Webseite!</h1>
        <p>Hallo Welt</p>
    </body>
</html>
');

// Finde den ersten Abschnitt (p)
$p = $doc->getElementsByTagName('p')->item(0);

// gibt den Inhalt des Abschnitts aus (Hallo Welt)
echo $p->textContent;
?>
```

Das wird "Hallo Welt" ausgeben.

## Deep Dive:
HTML-Parsing hat eine lange Geschichte und geht auf die Anfänge des Webs zurück. Im Laufe der Zeit hat sich der Prozess stets weiterentwickelt und verbessert.

Es gibt auch Alternativen zum Standard-DOMDocument in PHP, z.B. SimpleHTMLDom oder PHPQuery. Diese Tools bieten oft mehr Funktionen, können aber langsamer sein oder zusätzliche Abhängigkeiten haben.

Beim Parsen eines HTML-Dokuments mit der DOMDocument-Klasse wird eine Baumstruktur erstellt. Jedes Element im HTML wird zu einem Knoten in diesem Baum.

## Siehe auch:
1. Offizielle [PHP-Dokumentation](https://www.php.net/manual/de/book.dom.php)
2. [DOMDocument](https://www.php.net/manual/de/class.domdocument.php)
3. [SimpleHTMLDom](http://simplehtmldom.sourceforge.net/)
4. [PHPQuery](https://code.google.com/archive/p/phpquery/)