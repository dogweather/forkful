---
title:                "C#: Großschreibung einer Zeichenkette"
simple_title:         "Großschreibung einer Zeichenkette"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es immer wieder Aufgaben, die anfangs scheinbar simpel und unkompliziert erscheinen, sich aber bei genauerem Hinsehen als komplex herausstellen. Eine dieser vermeintlich einfachen Aufgaben ist das Großschreiben eines Strings. Warum sollte man sich überhaupt die Mühe machen, einen String zu capitalisieren? Die Antwort ist einfach: um die Lesbarkeit und Übersichtlichkeit von Texten zu verbessern.

## Wie geht das?

In C# gibt es eine eingebaute Funktion, die das Großschreiben eines Strings übernimmt. Diese Funktion heißt "ToUpper" und wird in der folgenden Codezeile verwendet:

```C#
string text = "hallo welt";
string capitalizedText = text.ToUpper();
```

Die Ausgabe des obigen Codes wäre "HALLO WELT". Wie man sieht, wandelt die Funktion "ToUpper" alle Buchstaben des Strings in Großbuchstaben um. Eine alternative Methode, um einen String zu capitalisieren, ist der Einsatz von Schleifen und bedingten Anweisungen. Hier ein Beispiel:

```C#
string text = "hallo welt";
string capitalizedText = "";

foreach (char c in text)
{
    if (Char.IsLower(c))
    {
        capitalizedText += char.ToUpper(c);
    }
    else
    {
        capitalizedText += c;
    }
}

Console.WriteLine(capitalizedText);
```

Dieses Beispiel nutzt die Methode "IsLower" aus der Klasse "Char", um zu überprüfen, ob ein Buchstabe kleingeschrieben ist. Ist dies der Fall, wird der Buchstabe in Großbuchstaben umgewandelt und der neuen Variable "capitalizedText" hinzugefügt. Ist der Buchstabe bereits großgeschrieben, wird er einfach übernommen. Die Ausgabe ist in beiden Fällen wieder "HALLO WELT".

## Tiefergehende Informationen

Das Capitalisieren von Strings mag auf den ersten Blick einfach erscheinen, aber es gibt einige Dinge zu beachten. Zum Beispiel kann die Methode "ToUpper" nicht mit Akzenten oder besonderen Zeichen umgehen, wodurch diese verloren gehen würden. Auch die Verwendung von Unicode-Zeichen kann zu Problemen führen. Deshalb ist es wichtig, sich vorher genau zu überlegen, welche Methode für das Capitalisieren eines Strings am besten geeignet ist.

## Siehe auch

- [Microsoft Dokumentation zur Methode "ToUpper"](https://docs.microsoft.com/de-de/dotnet/api/system.string.toupper?view=net-5.0)
- [C# Programmier-Tutorial auf Deutsch](https://csharpindepth.com/de/Articles)