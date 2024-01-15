---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "C#: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum
Es gibt verschiedene Gründe, warum man sich dafür entscheiden könnte, Zeichenfolgen in C# zu löschen, die einem bestimmten Muster entsprechen. Zum Beispiel können Sie möglicherweise unerwünschte Zeichen entfernen, um die Lesbarkeit Ihres Codes zu verbessern oder Daten zu bereinigen, bevor Sie sie verarbeiten.

## Wie geht's
Um Zeichenfolgen in C# zu löschen, die einem bestimmten Muster entsprechen, können Sie die `Regex` Klasse verwenden. Diese Klasse bietet Funktionen zum Suchen und Ersetzen von Text basierend auf einem regulären Ausdruck.
 
Ein Beispielcode könnte wie folgt aussehen:

```C#
// Erstellen Sie eine Instanz der Regex-Klasse und geben Sie das gewünschte Muster an.
Regex regex = new Regex("[aeiou]");

// Ersetzen Sie alle Vokale in einem Text durch Leerzeichen.
string text = "Hallo Welt";
string result = regex.Replace(text, " ");

Console.WriteLine(result);
// Output: H ll W lt
```

Dieses Beispiel zeigt, wie Sie mithilfe des `regex.Replace` -Befehls Zeichenfolgen entfernen können, die dem Muster `[aeiou]` entsprechen.

## Tiefer eintauchen
Die `Regex` Klasse bietet viele weitere Funktionen, die Sie erkunden können, um Ihre Anforderungen zu erfüllen. Sie können auch eine Vielzahl von Variationen des regulären Ausdrucks ausprobieren, um genau die Zeichenfolgen zu finden, die Sie löschen möchten.

Eine andere Möglichkeit, Zeichenfolgen basierend auf einem regulären Ausdruck zu löschen, ist die Verwendung des `Trim` Befehls. Dieser entfernt alle angegebenen Zeichen am Anfang und Ende einer Zeichenfolge. Sie können also beispielsweise das folgende Codebeispiel verwenden, um alle Leerzeichen am Anfang und Ende eines Textes zu entfernen:

```C#
string text = "       Hallo Welt       ";
string result = text.Trim();

Console.WriteLine(result);
// Output: "Hallo Welt"
```

## Siehe auch
 - [Microsoft Docs - Regex Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1)
 - [Microsoft Docs - Trim Methode](https://docs.microsoft.com/de-de/dotnet/api/system.string.trim?view=netcore-3.1)