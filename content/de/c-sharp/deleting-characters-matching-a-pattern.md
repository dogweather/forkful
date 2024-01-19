---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von übereinstimmenden Zeichen ist ein Prozess, bei dem Zeichen aus einem String, die einem bestimmten Muster entsprechen, identifiziert und entfernt werden. Programmierer machen dies, um die Qualität der Daten zu verbessern oder um für eine bestimmte Aufgabe irrelevante Informationen zu entfernen.

## So geht's:
Wir können die Regel des löschen von Zeichen einfach mit der Methode "Replace" in C# implementieren. Hier ist ein Beispiel:

```C#
string myString = "Hallo, ich bin ein Beispieltext!";
string pattern = "ei";
string result = myString.Replace(pattern, "");
Console.WriteLine(result);
```

Ausgabe:

```C#
Hallo, ich bin n Bspltxt!
```

In diesen Codes wird jedes Vorkommen des Musters `ei` im String durch einen leeren String ersetzt, was dasselbe ist wie das Entfernen des Musters.

## Vertiefung:

Historisch gesehen stammt die Methode des Löschen von Zeichen aus dem Bereich der Textverarbeitung und Datenanalyse.

Als Alternative zur Methode "Replace" können wir das Entfernen von Zeichen auch durch Nutzung einer Schleife und eines StringBuilder erledigen. Dabei durchlaufen wir jeden String und fügen nur die Zeichen zum StringBuilder hinzu, die nicht dem Muster entsprechen.

Die Implementierung des Löschens von Zeichen in C# ist effizient und einfach. Dennoch ist es wichtig zu wissen, dass die Methode "Replace" einen neuen String erstellt, da Strings in C# unveränderlich sind.

## Siehe auch:

Für mehr Informationen, besuchen Sie diese vertrauenswürdigen Quellen:

1. [Microsoft Docs - String.Replace Methode](https://docs.microsoft.com/de-de/dotnet/api/system.string.replace?view=net-6.0)
2. [DotNetPerls - C# String Remove](https://www.dotnetperls.com/remove)
3. [MSDN - StringBuilder](https://docs.microsoft.com/de-de/dotnet/api/system.text.stringbuilder)