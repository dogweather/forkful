---
title:    "PHP: Strings verketten"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Zusammenfügen von Strings (Zeichenketten) ist eine wichtige Grundfunktion in der Programmierung. Es ermöglicht es uns, mehrere Zeichenketten zu einer einzelnen Zeichenkette zu verbinden, was besonders nützlich ist, wenn wir dynamisch generierte Daten darstellen möchten.

## Wie man String-Ketten in PHP zusammenfügt

Die einfachste Methode, um Strings in PHP zu kombinieren, ist die Verwendung des Konkatenationsoperators (".") oder der Verkettungsfunktion "concat()". Schauen wir uns ein Beispiel an:

```PHP
$vorname = "Anna";
$nachname = "Müller";
$gruß = "Hallo, " . $vorname . " " . $nachname . "!";
echo $gruß;
```

Die Ausgabe dieses Codes ist "Hallo, Anna Müller!". Wir haben erfolgreich die Variablen "$vorname" und "$nachname" zusammengefügt und in der Variablen "$gruß" gespeichert, die dann ausgegeben wurde.

Eine andere Möglichkeit, Strings zu verketten, ist die Verwendung von "sprintf()". Diese Funktion ermöglicht es uns, Platzhalter zu verwenden, um Variablen in einer Zeichenkette zu ersetzen. Ein Beispiel:

```PHP
$anzahl = 5;
$produkt = "Bücher";
$preis = 10.99;
$gesamt = sprintf("%d %s für %.2f Euro", $anzahl, $produkt, $preis);
echo $gesamt;
```

Die Ausgabe dieses Codes ist "5 Bücher für 10,99 Euro". Wir haben die Variablen "$anzahl", "$produkt" und "$preis" in die Zeichenkette eingegeben und durch die Platzhalter ersetzt. Das Ergebnis ist eine gut formatierte Zeichenkette, die dann ausgegeben wird.

## Tiefer Einblick

Beim Zusammenführen von Strings in PHP müssen wir auf einige wichtige Dinge achten. Zum Beispiel können wir keine Strings mit Zahlen mit dem Konkatenationsoperator verketten, da PHP versuchen wird, den Zahlenwert als String zu interpretieren. In diesem Fall müssen wir die Funktion "strval()" verwenden, um die Zahl in einen String umzuwandeln. Ein Beispiel:

```PHP
$zahl = 25;
$text = "Die Zahl ist " + $zahl; // Fehlermeldung!
$text_correct = "Die Zahl ist " . strval($zahl); // Ausgabe: "Die Zahl ist 25"
```

Ein weiteres wichtiges Detail: Wenn wir Variablen innerhalb einer Zeichenkette durch Platzhalter ersetzen, müssen wir auf die richtige Schreibweise achten. Zum Beispiel muss %d für ganze Zahlen, %f für Gleitkommazahlen und %s für Strings verwendet werden. Eine vollständige Liste der Platzhalter und ihre Bedeutungen finden Sie in der offiziellen PHP-Dokumentation.

## Siehe auch

- [Offizielle PHP-Dokumentation über String-Konkatenation](https://www.php.net/manual/en/language.operators.string.php)
- [Tutorial zu String-Konkatenation in PHP](https://www.w3schools.com/php/php_string_concat.asp)
- [Video-Tutorial zu String-Konkatenation auf YouTube](https://www.youtube.com/watch?v=6it1x327rKU)