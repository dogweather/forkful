---
title:                "C#: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie professionell programmieren, wissen Sie wahrscheinlich bereits, dass es wichtig ist, alle möglichen Fehler in Ihrem Code zu beheben. Das kann eine Herausforderung sein, aber es gibt eine einfache Möglichkeit, um Fehler zu erkennen und zu beheben - das "Schreiben in den Standardfehler". In diesem Blogpost erfahren Sie, warum und wie Sie Fehler aus der Konsole in Ihre C#-Codebeispiele eingeben sollten.

## Wie geht das?

Um Fehler in Ihrem Code zu erkennen, müssen Sie Ihn in den Standardfehler stream schreiben, auch bekannt als "Standardfehlerausgabe" oder "stderr". Mit nur wenigen Zeilen Code können Sie diesen Stream in Ihre Konsole integrieren.

Lassen Sie uns ein einfaches Beispiel in C# betrachten:

```C#
using System;

namespace WritingToStdErr
{
    class Program
    {
        static void Main(string[] args)
        {
            //Ein Beispiel für eine Fehlerausgabe in die Konsole
            Console.Error.WriteLine("Oops! Ein Fehler ist aufgetreten.");
        }
    }
}
```

Die Ausgabe dieses Codes wird im Standardfehler stream "Oops! Ein Fehler ist aufgetreten." sein. Beachten Sie, dass wir hier `Console.Error.WriteLine` verwenden, um in den Standardfehler stream zu schreiben, anstatt `Console.WriteLine` zu ver