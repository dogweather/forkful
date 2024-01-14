---
title:    "C#: Eine Zeichenkette großschreiben"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum

Das Großschreiben von Strings ist eine häufig genutzte Funktion in der C# Programmierung. Es kann verwendet werden, um Benutzereingaben zu normalisieren, Datenbankabfragen zu vereinfachen oder einfach nur die Darstellung von Text zu ändern. Wenn du wissen möchtest, wie du Strings in C# großschreiben kannst, lies weiter!

## Wie Man

Das Großschreiben von Strings in C# ist einfach und unkompliziert. Hier ist ein Beispiel:

```C#
string text = "hallo welt";
string uppercaseText = text.ToUpper();

Console.WriteLine(uppercaseText);
```
Das obige Beispiel nimmt den ursprünglichen String "hallo welt" und schreibt ihn in Großbuchstaben um, was in der Ausgabe "HALLO WELT" resultiert.

Um das erste Zeichen eines Strings großzuschreiben, kannst du die Funktion "FirstCharToUpper" verwenden, wie im folgenden Beispiel gezeigt:

```C#
string text = "hallo welt";
string firstCharUppercase = FirstCharToUpper(text);

Console.WriteLine(firstCharUppercase);

string FirstCharToUpper(string str)
{
    if (string.IsNullOrEmpty(str))
        return string.Empty;
    return char.ToUpper(str[0]) + str.Substring(1);
}
```
Die Funktion "FirstCharToUpper" nimmt den ersten Buchstaben des Strings und wandelt ihn in einen Großbuchstaben um, während der Rest des Strings unverändert bleibt. Die Ausgabe hier wäre "Hallo welt".

Es gibt auch die Möglichkeit, Strings mit Hilfe der Funktion "String.Format" in Großbuchstaben umzuwandeln:

```C#
string text = "hallo welt";
string uppercaseText = string.Format("{0}", text).ToUpper();

Console.WriteLine(uppercaseText);
```

## Deep Dive

Beim Großschreiben von Strings solltest du einige Dinge beachten. Zum Beispiel ist es wichtig zu verstehen, dass in C# Strings unveränderlich sind, was bedeutet, dass sie nicht direkt geändert werden können. Stattdessen wird bei jeder Änderung ein neues String-Objekt erstellt. Deshalb ist es effizienter, die Funktionen "ToUpper" oder "FirstCharToUpper" zu verwenden, anstatt den String manuell zu ändern.

Außerdem kann das Großschreiben von Strings in verschiedenen Kulturkontexten unterschiedliche Ergebnisse liefern, da einige Sprachen besondere Regeln für Großbuchstaben haben. In solchen Fällen ist es empfehlenswert, die überladene Version der Funktion "ToUpper" zu verwenden, die einen CultureInfo-Parameter akzeptiert, um das gewünschte Verhalten zu spezifizieren.

## Siehe auch

- [C# String-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.string?view=netcore-3.1)
- [Kulturabhängige Strings in C#](https://docs.microsoft.com/de-de/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1)
- [String Manipulation in C#](https://www.tutorialspoint.com/csharp/csharp_string_manipulation.htm)