---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Verkettung von Zeichenketten (String Concatenation) spielt eine Schlüsselrolle in PHP, weil man damit mehrere Zeichenketten zu einer einzigen zusammenfügt. Es ist der Hauptweg, Text in dynamischen Inhalten zu verwalten.

## So geht's:

Du kannst den Punkt (.) Operator zum Verketten von Strings in PHP benutzen. Schau Dir das folgende Beispiel an:

```PHP
<?php
$teil1 = "Hallo";
$teil2 = ", Welt!";
$satz = $teil1 . $teil2; // Verkettung
echo $satz;
?>
```

Die Ausgabe wird sein:

```
Hallo, Welt!
```

## Vertiefung

1. Historischer Kontext: Die Verkettung von Strings wurde in PHP eingeführt, um komplexere Datenstrukturen zu ermöglichen und die Anzeige dynamischer Inhalte zu erleichtern.

2. Alternativen: Eine Alternative zur Verkettung von Strings ist die Verwendung von doppelt-angezogenen Zeichenketten, wo die Variablen innerhalb der Zeichenkette deklariert werden, z.B. `$satz = "$teil1$teil2";`. Mit dem operator `.=`, kann man den Inhalt einer Variablen direkt erweitern: `$satz .= $weiterer_Text;`.

3. Implementierungsdetails: Intern verwendet PHP einen Mechanismus namens "Copy on Write", der optimiert, wie Speicherplatz bei der Verkettung von Strings verwendet wird.

## Siehe auch:

1. Die PHP-Dokumentation für Zeichenketten: [PHP: Strings - Manual](https://www.php.net/manual/de/language.types.string.php)