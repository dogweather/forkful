---
title:                "Herunterladen einer Webseite"
html_title:           "PHP: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite bedeutet, dass man sich den Inhalt der Seite auf seinen Computer lädt, um ihn später offline nutzen zu können. Programmierer nutzen dieses Verfahren beispielsweise, um automatisiert Daten von anderen Webseiten zu sammeln oder um Änderungen an bestehenden Webseiten zu überwachen.

## Wie geht's?

Um eine Webseite mit PHP herunterzuladen, muss man zunächst die Funktion ```file_get_contents()``` verwenden. In den folgenden Beispielen werden wir die Webseite von Google herunterladen und deren Inhalt ausgeben.

```
<?php

// Webseite herunterladen
$webseite = file_get_contents("https://www.google.de");

// Webseite ausgeben
echo $webseite;
```

Dieses einfache Beispiel lädt die Google-Startseite herunter und gibt sie aus. Man kann auch bestimmte Teile der Webseite auswählen, indem man spezifische Elemente wie zum Beispiel ```<title>``` oder ```<img>``` mithilfe der Funktion `file_get_html()` auswählt. Mehr Informationen dazu findet man in der Dokumentation von PHP.

## Tiefere Einblicke

Das Herunterladen von Webseiten hat eine lange Geschichte und wurde zuerst in Perl eingeführt. Auch in anderen Programmiersprachen wie Python oder Java gibt es ähnliche Funktionen.

Eine Alternative zur Verwendung von `file_get_contents()` ist die Verwendung von cURL, einer Erweiterung für PHP. Diese bietet zusätzliche Funktionen wie beispielsweise die Unterstützung von HTTPS und Passwortschutz.

Die Implementierung des Herunterladens von Webseiten mit PHP ist relativ einfach, da die Funktion ```file_get_contents()``` vom Betriebssystem unterstützt wird. Das bedeutet, dass man keine zusätzlichen Bibliotheken installieren muss.

## Siehe auch

Weitere Informationen zu `file_get_contents()` und anderen nützlichen PHP-Funktionen findet man in der offiziellen PHP-Dokumentation: https://www.php.net/manual/en/function.file-get-contents.php

Für fortgeschrittenere Techniken zur Verarbeitung von Webseiten mit PHP empfehle ich die folgenden Bibliotheken:

- Simple HTML DOM: http://simplehtmldom.sourceforge.net/
- Goutte: https://github.com/FriendsOfPHP/Goutte