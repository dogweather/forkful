---
title:    "Fish Shell: Verwendung regulärer Ausdrücke"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Warum

Fish Shell ist eine leistungsstarke und benutzerfreundliche Shell, die es Benutzern ermöglicht, effektiv mit ihren Dateien und Systemen zu interagieren. Eine ihrer nützlichsten Funktionen ist die Unterstützung von regulären Ausdrücken (Regular Expressions, oder kurz "Regex"). Durch die Verwendung von Regex können Benutzer komplexe Suchanfragen in ihren Dateien und Systemen durchführen. In diesem Blogbeitrag werden wir uns genauer ansehen, warum die Verwendung von Regex in Fish Shell so hilfreich ist.

## Wie man reguläre Ausdrücke in Fish Shell verwendet

### Einfache Suche

Die grundlegendste Funktion von regulären Ausdrücken ist die Möglichkeit, bestimmte Zeichenfolgen in Dateien zu suchen. Angenommen, wir haben eine Datei mit den Namen von Personen und ihren dazugehörigen E-Mail-Adressen. Wir können versuchen, nur die E-Mail-Adressen aus dieser Datei herauszufinden, indem wir einen regulären Ausdruck verwenden.

```
Fish Shell (Ja, lisp ...

# Greife auf die Datei mit den Informationen zu
set file (cat Personen.txt)

# Suche nach allen E-Mail-Adressen
echo $file | grep -Eo '[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+'
```

Der obige Code verwendet den `grep`-Befehl, um alle Zeilen in der Datei zu finden, die dem angegebenen regulären Ausdruck entsprechen. In diesem Fall suchen wir nach einer Zeichenfolge, die einem typischen E-Mail-Adressformat entspricht. Das `-E`-Flag ermöglicht die Verwendung von regulären Ausdrücken, während das `-o`-Flag nur die übereinstimmenden Teile der Zeilen ausgibt.

### Ersetzung

Beispielweise möchten wir alle Punkte aus den E-Mail-Adressen in unserer Datei entfernen und stattdessen Kommas verwenden. Mit regulären Ausdrücken ist dies auch möglich.

```
# Ersetze alle '.' mit ',' in den E-Mail-Adressen
echo $file | sed -E 's/\.+/,/g'
```

In diesem Beispiel verwenden wir den `sed`-Befehl, um alle '.' mit ',' in jeder Zeile zu ersetzen, die dem regulären Ausdruck entspricht. Das `-E`-Flag ermöglicht es uns, einen regulären Ausdruck als Muster für die Ersetzung zu verwenden.

## Ein tieferer Einblick in reguläre Ausdrücke

Reguläre Ausdrücke in Fish Shell verwenden eine Syntax ähnlich der von vielen anderen Programmiersprachen. Sie bestehen aus verschiedenen Zeichen und speziellen Symbolen, die es ermöglichen, komplexe Zeichenfolgen zu definieren.

Zum Beispiel können wir einen regulären Ausdruck verwenden, um alle Wörter in einem Text zu zählen.

```
echo "Dies ist ein Text zum Testen." | wc -w | grep -Eo '[0-9]+'
```

Dieser Code verwendet `wc` (word count), um die Anzahl der Wörter in einem Text auszugeben und dann `grep`, um nur die Zahl auszugeben. Der reguläre Ausdruck `[0-9]+` findet alle Zahlen in der Ausgabe von `wc `und gibt sie aus.

## Siehe auch

- Fish Shell-Dokumentation: [Reguläre Ausdrücke](https://fishshell.com/docs/current/tutorial.html#regular-expressions)
- Einführung in reguläre Ausdrücke: [Ein Leitfaden von Google Developers](https://developers.google.com/edu/python/regular-expressions)
- Reguläre Ausdrücke bei Wikipedia: [Ein Artikel über reguläre Ausdrücke und ihre Verwendung](https://de.wikipedia.org/wiki/Regul%C3%A4rer_Ausdruck)