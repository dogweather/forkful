---
title:                "Umwandlung eines Strings in Kleinbuchstaben."
html_title:           "PHP: Umwandlung eines Strings in Kleinbuchstaben."
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben."
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum

Wenn Sie jemals mit PHP gearbeitet haben oder eine Website programmiert haben, haben Sie wahrscheinlich schon einmal eine Situation erlebt, in der Sie einen Kollisions-Detection-Algorithmus erstellen müssen. In manchen Fällen müssen Sie dabei möglicherweise einen Text in Kleinbuchstaben (z.B. "Hallo Welt") umwandeln, um ihn einfacher vergleichen zu können. In diesem Artikel werde ich Ihnen zeigen, wie Sie ganz einfach einen String in Kleinbuchstaben umwandeln können.

# Wie geht das?

Die Funktion, die wir verwenden werden, ist `strtolower()`. Hier ist ein Beispiel, welches unsere Eingabe (ein Name) in Kleinbuchstaben umwandelt:

```PHP
$name = "Max Mustermann";
strtolower($name);
```

Das ist alles, was Sie tun müssen! Die Funktion gibt das Ergebnis, also den umgewandelten Text, als Rückgabewert aus. Hier ist das Ergebnis unseres Beispiels: "max mustermann".

Die `strtolower()` Funktion ist auch hilfreich, wenn Sie Benutzereingaben validieren müssen und sicherstellen möchten, dass ein Text in einer einheitlichen Schreibweise vorliegt, bevor er in einer Datenbank gespeichert wird.

# Tiefergehende Informationen

Die `strtolower()` Funktion wandelt nicht nur die Buchstaben A-Z in a-z um, sondern auch alle Sonderzeichen, die im ASCII-Zeichensatz enthalten sind (unabhängig von der Zeichencodierung Ihrer Website). Das bedeutet, dass Sie beim Vergleichen von Zeichenfolgen auch die `strtolower()` Funktion verwenden müssen, um sicherzustellen, dass die Sonderzeichen richtig verglichen werden.

Eine weitere wichtige Sache, die Sie berücksichtigen sollten, ist die Lokalisierung. Die `strtolower()` Funktion kann je nach Sprachumgebung unterschiedliche Ergebnisse liefern. Um dies zu vermeiden, können Sie die `setlocale()` Funktion verwenden, um die richtige Sprachumgebung für Ihren Code zu definieren.

# Siehe auch

Hier sind einige weitere nützliche Links rund um PHP und die `strtolower()` Funktion:

- [Offizielle PHP-Dokumentation zu strtolower()](https://www.php.net/manual/de/function.strtolower.php)
- [Tutorials zu PHP von Codecademy](https://www.codecademy.com/learn/learn-php)
- [String manipulation in PHP: A guide for beginners (Englisch)](https://www.keycdn.com/blog/php-string-manipulation)