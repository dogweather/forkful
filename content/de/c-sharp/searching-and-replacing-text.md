---
title:                "C#: Textsuche und -ersetzung"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Warum

Das Suchen und Ersetzen von Text ist eine wichtige Fähigkeit, wenn es um die Bearbeitung von Dateien oder das Schreiben von Code geht. Es ermöglicht es uns, schnell und effizient bestimmte Textpassagen zu finden und zu ändern, anstatt dies manuell tun zu müssen.

# Wie geht's

Um Text in C# zu suchen und zu ersetzen, verwenden wir die `Replace()` Methode. Diese Methode erwartet zwei Parameter: den zu ersetzenden Text und den Text, mit dem er ersetzt werden soll. Sehen wir uns ein Beispiel an:

```C#
string text = "Hallo Welt!";
string newText = text.Replace("Hallo", "Guten Tag");
Console.WriteLine(newText);
```

Output: `Guten Tag Welt!`

In diesem Beispiel haben wir den Text "Hallo" durch "Guten Tag" ersetzt und das Ergebnis in der Variablen `newText` gespeichert.

Um jedoch in einer Datei oder einem längeren Text nach bestimmten Wörtern zu suchen und zu ersetzen, können wir die `Regex` Klasse verwenden. Diese Klasse ermöglicht es, mithilfe von regulären Ausdrücken spezifischere Suchmuster zu definieren.

Schauen wir uns ein Beispiel an, in dem wir alle Vokale in einem Text durch einen Unterstrich ersetzen:

```C#
string text = "Ich liebe Programmieren!";
string pattern = "[aeiou]";
string newText = Regex.Replace(text, pattern, "_");
Console.WriteLine(newText);
```

Output: `_ch l_b_ Pr_gr_mm_r_n!`

In diesem Beispiel verwenden wir `[aeiou]` als Suchmuster, was bedeutet, dass alle Vokale in dem Text durch einen Unterstrich ersetzt werden.

Wir können auch Platzhalter verwenden, um bestimmte Wörter oder Zeichenfolgen zu finden und zu ersetzen. Schauen wir uns ein Beispiel an, in dem wir alle Zahlen im Text durch Sternchen ersetzen:

```C#
string text = "Heute ist der 3. Februar";
string pattern = @"\d";
string newText = Regex.Replace(text, pattern, "*");
Console.WriteLine(newText);
```

Output: `Heute ist der *. Februar`

In diesem Beispiel verwenden wir den Platzhalter `\d`, der für eine beliebige Zahl steht.

# Deep Dive

Suchen und Ersetzen von Text mag einfach erscheinen, aber es kann auch komplexere Anwendungen haben. Eine weitere nützliche Funktion, die die `Regex` Klasse bietet, ist die Möglichkeit, Text zu extrahieren oder zu ersetzen, der bestimmten Kriterien entspricht. Zum Beispiel könnten wir alle URLs in einem Text finden und in anklickbare Links umwandeln oder die E-Mail-Adressen in einem Text in eine bestimmte Formatierung bringen.

Die Verwendung von regulären Ausdrücken erfordert möglicherweise etwas Übung, aber sie kann eine leistungsstarke Fähigkeit sein, um effizient mit Text zu arbeiten.

# Siehe auch

- [Offizielle Dokumentation zu C# String.Replace()](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netframework-4.8)
- [Tutorial zu regulären Ausdrücken in C#](https://www.w3schools.com/cs/cs_regex.asp)
- [RegExr - Online-Tool zum Testen von regulären Ausdrücken](https://regexr.com/)