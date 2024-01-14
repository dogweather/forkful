---
title:    "PHP: Großschreibung eines Strings"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum
In der Programmierung gibt es oft die Notwendigkeit, bestimmte Zeichenfolgen zu formatieren, um sie lesbarer oder ästhetisch ansprechender zu machen. Eine häufige Formatierung ist die Großschreibung von Strings. In diesem Blogbeitrag erfahren Sie, warum diese Formatierung nützlich sein kann.

## Wie geht's
Um einen String in PHP zu Großbuchstaben zu konvertieren, können wir die Funktion `strtoupper()` verwenden. Dies ist besonders hilfreich, wenn wir Benutzereingaben normalisieren möchten, um beispielsweise sicherzustellen, dass alle Suchanfragen in Großbuchstaben vorliegen. Im Folgenden ein Beispielcode:

```PHP
$string = "Hallo, wie geht es dir?";
echo strtoupper($string);
```

Die Ausgabe sieht wie folgt aus:

```PHP
HALLO, WIE GEHT ES DIR?
```

Wir können auch `ucwords()` verwenden, um jeden Anfangsbuchstaben in einem String zu groß zu schreiben. Schauen wir uns ein weiteres Beispiel an:

```PHP
$string = "hello world";
echo ucwords($string);
```

Die Ausgabe sieht folgendermaßen aus:

```PHP
Hello World
```

Diese Funktionen können auch nützlich sein, wenn wir Datenbankabfragen ausführen und sicherstellen müssen, dass bestimmte Felder immer in Großbuchstaben vorliegen.

## Tiefere Einblicke
Während die Großschreibung eines Strings ein einfacher Schritt in der Programmierung sein mag, ist es immer wichtig, die Auswirkungen auf die Performance zu beachten. In PHP werden Strings intern als Arrays behandelt, daher kann die Verwendung von `strtoupper()` oder `ucwords()` auf sehr langen Strings zusätzliche Ressourcen erfordern.

Eine alternative Möglichkeit, Strings in Großbuchstaben zu konvertieren, besteht darin, die Sprachfunktion `mb_convert_case()` zu verwenden. Diese Funktion ermöglicht es uns, den gewünschten Ausgabe- und Eingabezeichensatz zu definieren, was besonders hilfreich sein kann, wenn wir mit Nicht-ASCII-Strings oder mehrsprachigen Anwendungen arbeiten.

## Siehe auch
- [Die offizielle PHP-Dokumentation zu strtoupper()](https://www.php.net/manual/de/function.strtoupper.php)
- [Die offizielle PHP-Dokumentation zu ucwords()](https://www.php.net/manual/de/function.ucwords.php)
- [Die offizielle PHP-Dokumentation zu mb_convert_case()](https://www.php.net/manual/de/function.mb-convert-case.php)