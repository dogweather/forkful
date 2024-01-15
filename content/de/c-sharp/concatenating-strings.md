---
title:                "Zusammenfügen von Zeichenketten"
html_title:           "C#: Zusammenfügen von Zeichenketten"
simple_title:         "Zusammenfügen von Zeichenketten"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum

Das Verketten von Strings ist eine grundlegende Operation in der Programmierung, die es uns ermöglicht, mehrere Textteile miteinander zu verbinden und so dynamische Inhalte zu generieren. Es wird oft verwendet, um Benutzern personalisierte Nachrichten anzuzeigen oder Dateipfade in unserem Code zu erstellen.

# Wie geht das

Um Strings in C# zu verketten, können wir entweder den "+" Operator oder die "string.Format" Methode verwenden. Hier ist ein kurzes Beispiel:

```C#
string name = "David";
string message1 = "Hallo, mein Name ist " + name + "!";
string message2 = string.Format("Schön dich kennenzulernen, {0}!", name);
Console.WriteLine(message1);
Console.WriteLine(message2);
```
***Ausgabe:***
Hallo, mein Name ist David!  
Schön dich kennenzulernen, David!

Beide Methoden erzielen das gleiche Ergebnis, aber die "string.Format" Methode bietet uns mehr Flexibilität, da wir Platzhalter verwenden können, um mehrere Variablen in einem String zu verketten. Hier ist ein weiteres Beispiel:

```C#
string city = "Berlin";
int temperature = 20;
string weather = "sonnig";
string forecast = string.Format("Willkommen in {0}! Wir haben heute {1} Grad und es ist {2}.", city, temperature, weather);
```
***Ausgabe:***
Willkommen in Berlin! Wir haben heute 20 Grad und es ist sonnig.

Es ist auch möglich, mehrere Strings mit dem "string.Join" Befehl zu verketten, der uns erlaubt, ein Trennzeichen zwischen den Werten anzugeben. Hier ist ein Beispiel:

```C#
string[] names = {"Anna", "Bob", "Claire"};
string message = string.Join(", ", names);
Console.WriteLine(message);
```
***Ausgabe:***
Anna, Bob, Claire

# Tiefer Einblick

In C# werden Strings als Objekte behandelt, was bedeutet, dass jeder String immutable ist, d.h. er kann nicht geändert werden. Wenn wir also Strings verketten, erstellen wir tatsächlich jedes Mal ein neues String-Objekt im Speicher.

Um dies zu vermeiden, können wir den "StringBuilder" verwenden, der speziell für das Verketten von Strings entwickelt wurde und ermöglicht, dass Änderungen an einem String direkt an diesem Objekt vorgenommen werden, wodurch der Speicheraufwand verringert wird. Hier ist ein Beispiel, wie wir den "StringBuilder" verwenden können:

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Dies ist ");
sb.Append("ein Beispiel ");
sb.Append("für den StringBuilder.");
string message = sb.ToString();
Console.WriteLine(message);
```
***Ausgabe:***
Dies ist ein Beispiel für den StringBuilder.

Es ist wichtig zu beachten, dass in den meisten Fällen die Verwendung von "string.Format" oder "string.Join" mehr als ausreichend ist und der "StringBuilder" nur in speziellen Fällen benötigt wird, in denen wir mit einer großen Anzahl von Strings arbeiten.

# Siehe auch

- [Microsoft Documentation: String Concatenation](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/#concatenation)
- [Microsoft Documentation: String.Join Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.join)