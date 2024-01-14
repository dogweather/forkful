---
title:                "PHP: Textsuche und -ersetzung"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum
Das Suchen und Ersetzen von Text ist eine grundlegende Funktion in der Programmierung. Es ermöglicht es uns, schnell und effizient große Mengen an Text zu bearbeiten und anzupassen. Wenn du nach Möglichkeiten suchst, deine Textbearbeitungsprozesse zu optimieren, ist das Erlernen dieser Fähigkeit definitiv die Mühe wert.

## Wie geht's
Das Suchen und Ersetzen von Text in PHP ist relativ einfach. Alles, was du brauchst, ist die Funktion `str_replace()`, die dafür entworfen wurde, einen bestimmten Text in einem anderen Text zu suchen und ihn durch einen neuen Text zu ersetzen. Schauen wir uns ein Beispiel an:

```PHP
$original_text = "Heute ist ein schöner Tag.";
$new_text = str_replace("schöner", "perfekter", $original_text);
echo $new_text;
```

Die Ausgabe wäre:

```
Heute ist ein perfekter Tag.
```

In diesem Beispiel haben wir den Text "schöner" durch "perfekter" ersetzt und die veränderte Version des Textes ausgegeben. Du kannst auch mehrere Such- und Ersetzungspaare angeben, um komplexe Textänderungen durchzuführen.

## Tiefere Einblicke
Die `str_replace()` Funktion unterstützt auch den Einsatz von Arrays, wodurch du mehrere Such- und Ersetzungspaare auf einmal angeben kannst. Außerdem gibt es eine Vielzahl von Optionen, um die Suche und das Ersetzen anzupassen, z.B. die Groß-/Kleinschreibung zu ignorieren oder nur ganze Wörter zu ersetzen.

Eine weitere hilfreiche Funktion ist `preg_replace()`, die reguläre Ausdrücke unterstützt und somit noch leistungsfähiger ist bei der Suche und dem Ersatz von Text.

## Siehe auch
- PHP-Handbuch zu `str_replace()`: https://www.php.net/manual/de/function.str-replace.php
- PHP-Handbuch zu `preg_replace()`: https://www.php.net/manual/de/function.preg-replace.php
- Reguläre Ausdrücke in PHP: https://www.php.net/manual/en/pcre.pattern.php