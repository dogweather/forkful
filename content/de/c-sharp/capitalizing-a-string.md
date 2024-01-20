---
title:                "Einen String großschreiben"
html_title:           "C#: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Großbuchstaben in einer Zeichenkette (engl. "string") bedeutet, alle kleingeschriebenen Zeichen in Großbuchstaben zu konvertieren. Programmierer verwenden es, um die Lesbarkeit zu verbessern oder Zeichenkettenvergleiche zu vereinfachen.

## Wie geht das?

Mit C# können Sie eine Zeichenkette einfach mit der `ToUpper()` Methode in Großbuchstaben konvertieren. Hier ist ein einfaches Beispiel:

```C#
  string kleingeschrieben = "hallo welt";
  string großgeschrieben = kleingeschrieben.ToUpper();

  Console.WriteLine(großgeschrieben);
  // Ausgabe: HALLO WELT
```

In diesem Code wird unser ursprünglicher kleingeschriebener Text zur "Hallo-Welt"-Zeichenkette und dann in Großbuchstaben umgewandelt.

## Tieferer Einblick

Die `ToUpper()` Methode ist in C# seit .NET Framework 1.1 enthalten und ist seitdem die grundlegende Methode zur Konvertierung von Zeichenketten in Großbuchstaben. 

Es gibt Alternativen, wie die `TextInfo.ToTitleCase()` Methode, die jedoch nur das erste Zeichen jedes Wortes groß macht und nicht wirklich dasselbe ist. 

Die `ToUpper()` Methode verwendet im Hintergrund eine Zuordnungstabelle, um die entsprechenden Großbuchstaben aus kleinen Zeichen zu ermitteln.  Sie ist auch kulturgebunden, d.h., sie konvertiert Zeichen basierend auf den kulturspezifischen Regeln der aktuellen oder angegebenen Kultur.

```C#
  string kleingeschrieben = "hallo welt";
  string großgeschrieben = kleingeschrieben.ToUpper(new CultureInfo("de-DE"));

  Console.WriteLine(großgeschrieben);
  // Ausgabe: HALLO WELT
```

## Weiterführende Links

[Microsoft Dokumentation zur ToUpper Methode](https://docs.microsoft.com/de-de/dotnet/api/system.string.toupper?view=net-5.0)

[Microsoft Dokumentation zur TextInfo.ToTitleCase Methode](https://docs.microsoft.com/de-de/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0)