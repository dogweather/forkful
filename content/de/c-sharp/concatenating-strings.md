---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die String-Konkatenierung ist der Prozess, mehrere Strings zu einem zusammenzufügen. Programmierer tun dies, um Text an einen bestimmten Ort zu formatieren oder Informationen sauber zu präsentieren.

## Wie zu:
Hier sind grundlegende Beispiele, wie man Strings in C# verkettet:

```C#
string str1 = "Hallo, ";
string str2 = "Welt!";
string zusammen = str1 + str2;
Console.WriteLine(zusammen);
```

Das Ausgabe werden sein:

```
Hallo, Welt!
```

Man kann auch die Methode `String.Concat` verwenden:

```C#
string zusammen = String.Concat(str1, str2);
Console.WriteLine(zusammen);
```

Die Methode `StringBuilder.Append` kann ins Spiel kommen, wenn die Performance bei mehrfacher Konkatenierung wichtig ist:

```C#
StringBuilder sb = new StringBuilder();
sb.Append(str1).Append(str2);
Console.WriteLine(sb.ToString());
```

## Tiefer Tauchgang
Historisch gesehen wurde String-Konkatenierung bereits in den ersten Hochsprachen verwendet. In C# und anderen .NET-Sprachen gibt es mehrere Methoden zur String-Konkatenierung, von denen jede ihre Vor- und Nachteile hat.

Die einfachste Methode ist der `+` Operator, aber er kann bei langen Sequenzen ineffizient sein, weil bei jeder Operation ein neuer String erzeugt wird.

Eine effizientere Methode für mehrere Konkatenierungen ist die Verwendung der `StringBuilder`-Klasse, die einen veränderbaren String-Buffer bereitstellt. Sie ist besonders nützlich, wenn Sie viele Manipulationen durchführen, da sie weniger Speicherzuweisungen und Kopiervorgänge durchführt als die direkte Konkatenierung.

Bedenken Sie, dass die Wahl der Methode von der spezifischen Anforderung abhängt. Es gibt keinen "one-size-fits-all"-Ansatz.

## Siehe Auch
- Microsoft's C#-Dokumentation: [String-Konkatenierung](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/strings/)
- Mehr zu [StringBuilder](https://docs.microsoft.com/de-de/dotnet/api/system.text.stringbuilder)
- StackOverflow Diskussion: ["Wie verkette ich Strings in .NET effizient?"](https://stackoverflow.com/questions/585860/whats-the-most-efficient-way-to-concatenate-strings-in-net)