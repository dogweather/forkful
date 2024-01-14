---
title:                "PHP: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
In der Welt der Programmierung gibt es oft Situationen, in denen man einen Text oder eine Zeichenfolge in Kleinbuchstaben umwandeln muss. Dies kann aus verschiedenen Gründen geschehen, zum Beispiel um die Vergleichbarkeit von Strings zu erhöhen oder um sicherzustellen, dass ein Text in einer bestimmten Formatierung angezeigt wird. In diesem Blogbeitrag werden wir uns anschauen, wie man in PHP eine Zeichenfolge in Kleinbuchstaben umwandeln kann und warum es nützlich sein kann, dies zu tun.

## Wie geht das?
Um eine Zeichenfolge in Kleinbuchstaben umzuwandeln, können wir die PHP-Funktion "strtolower()" verwenden. Diese Funktion nimmt einen Parameter entgegen, nämlich die zu konvertierende Zeichenfolge, und gibt diese in komplett kleinbuchstabierter Form zurück. Hier ein kleines Beispiel:

```php
$text = "Hallo Welt!";
echo strtolower($text);
```

Das oben genannte Beispiel würde als Ausgabe "hallo welt!" erzeugen. Wie man sieht, werden alle Buchstaben in Kleinbuchstaben umgewandelt, inklusive des Ausrufezeichens. Man sollte jedoch beachten, dass dies keine permanente Änderung ist, sondern nur für die Dauer der Ausführung des Skripts gültig ist.

## Tiefere Einblicke
Das Konvertieren einer Zeichenfolge in Kleinbuchstaben mag auf den ersten Blick trivial erscheinen, doch es gibt einige interessante Aspekte, die man vielleicht nicht unbedingt auf Anhieb bedenken würde. Zum Beispiel kann die Funktion "strtolower()" auch nicht-englische Zeichen richtig umwandeln, was bei manchen anderen Programmiersprachen nicht immer der Fall ist. Außerdem wird bei der Umwandlung automatisch auf die lokalisierten Einstellungen der PHP-Installation geachtet, was bei der richtigen Ausgabe von Sonderzeichen helfen kann.

## Siehe auch
- [PHP: strtolower() – Eine Dokumentation der PHP-Funktion](https://www.php.net/manual/de/function.strtolower.php)
- [PHP-Stammtisch Berlin: Eine Community rund um PHP in der deutschen Hauptstadt](https://php.berlin/)