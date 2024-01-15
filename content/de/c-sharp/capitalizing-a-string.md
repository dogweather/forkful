---
title:                "String großschreiben"
html_title:           "C#: String großschreiben"
simple_title:         "String großschreiben"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Zeichenketten ist eine häufige Operation in der Programmierung, die dazu dient, Texte einheitlich und lesbar zu gestalten. Es ist besonders nützlich, wenn Daten von externen Quellen übernommen werden, da diese möglicherweise nicht in der gewünschten Formatierung vorliegen.

## Wie

Um eine Zeichenkette in C# zu kapitalisieren, gibt es zwei grundlegende Ansätze. Der erste Ansatz ist die Verwendung der eingebauten Funktion "ToUpper()", die die gesamte Zeichenkette in Großbuchstaben umwandelt. Dies funktioniert jedoch nicht immer korrekt, da Umlaute oder Sonderzeichen möglicherweise nicht in der gewünschten Form konvertiert werden.

```
string text = "hallo welt!";
string capitalizedText = text.ToUpper();
Console.WriteLine(capitalizedText);
```

Output: HALLO WELT!

Für eine präzisere Konvertierung kann die Funktion "CultureInfo" verwendet werden, die die aktuelle Kultur des Systems berücksichtigt und auch Sonderzeichen korrekt umwandelt.

```
string text = "hallo welt!";
string capitalizedText = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(text);
Console.WriteLine(capitalizedText);
```

Output: Hallo Welt!

## Deep Dive

Die Funktion "CultureInfo" basiert auf den Regeln der aktuellen Kultur des Betriebssystems, die möglicherweise nicht immer den gewünschten Anforderungen entsprechen. In diesen Fällen kann die Verwendung einer benutzerdefinierten Methode nützlich sein, um die Zeichenkette nach eigenen Regeln zu kapitalisieren.

Ein Beispiel für eine einfache benutzerdefinierte Methode, die einen einzelnen String als Parameter annimmt und die erste Buchstabe in einen Großbuchstaben umwandelt:

```
public static string CapitalizeString(string text)
{
    if (string.IsNullOrEmpty(text))
        return text;

    text = text.ToLower();
    char firstLetter = char.ToUpper(text[0]);
    return firstLetter + text.Substring(1);
}
```

## Siehe auch

- [ToUpper() Methode](https://docs.microsoft.com/de-de/dotnet/api/system.string.toupper?view=net-5.0)
- [ToTitleCase() Methode](https://docs.microsoft.com/de-de/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0)
- [CultureInfo Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.globalization.cultureinfo?view=net-5.0)