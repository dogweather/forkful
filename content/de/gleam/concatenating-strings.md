---
title:                "Gleam: Verknüpfung von Zeichenketten"
simple_title:         "Verknüpfung von Zeichenketten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum
Ein wichtiger Teil des Programmierens ist die Manipulation von Text. Eine entscheidende Fähigkeit in dieser Manipulation ist das Verbinden von Strings, auch als "Concatenation" bezeichnet. Aber warum ist es überhaupt wichtig, das zu tun?

Wenn Sie beispielsweise eine Webanwendung schreiben, müssen Sie möglicherweise verschiedene Textfetzen zu einer vollständigen Nachricht zusammenfügen. Oder Sie möchten eine personalisierte E-Mail an Ihre Nutzer senden, in der bestimmte Textteile mit individuellen Informationen kombiniert werden. In all diesen Situationen ist das Verbinden von Strings unerlässlich.

In dieser kurzen "How To"-Anleitung werde ich Ihnen zeigen, wie Sie in Gleam Strings miteinander verbinden können.

## Wie geht das?
Um Strings in Gleam zu verbinden, können Sie das `<>`-Operator verwenden. Dieser Operator akzeptiert zwei Strings als Parameter und gibt einen neuen String zurück, der beide miteinander verbunden hat.

Ein Beispiel dazu in Gleam:

```
Gleam> name = "Max"
Gleam> greeting = "Hallo "
Gleam> message = greeting <> name
Gleam> IO.puts(message)
"Hello Max"
```

Hier haben wir den Operator verwendet, um `greeting` und `name` miteinander zu verbinden und den neuen String `message` zu erstellen. Wenn wir `message` ausgeben, erhalten wir die Nachricht "Hallo Max".

Sie können auch mehr als zwei Strings miteinander verbinden, indem Sie den Operator mehrmals hintereinander verwenden oder die `concat`-Funktion verwenden.

## Deep Dive
Um noch tiefer in das Thema der String-Verknüpfung einzutauchen, ist es wichtig zu verstehen, dass Strings in Gleam unveränderliche Werte sind. Das bedeutet, dass wenn Sie Strings miteinander verbinden, tatsächlich neue Strings erstellt werden. Die ursprünglichen Strings bleiben unberührt.

Außerdem sollten Sie beachten, dass der `<>`-Operator nur Strings miteinander verbinden kann. Wenn Sie andere Datentypen, wie z.B. Zahlen oder Booleans, kombinieren möchten, müssen Sie diese vorher in Strings umwandeln.

In komplexeren Anwendungen kann es auch sinnvoll sein, eine spezielle Funktion für die String-Verknüpfung zu erstellen, um die Wartbarkeit des Codes zu verbessern.

## Siehe auch
- Die offizielle Gleam-Dokumentation zu Strings: https://gleam.run/articles/strings
- Weitere Beispiele für String-Verknüpfung in Gleam: https://gist.github.com/mrvisser/4ed9de0af1950fde890351ec30f4fde0

*Möchten Sie mehr über die Funktionalität von Gleam erfahren? Schauen Sie sich doch auch unsere anderen Blog-Einträge an!*

*Möchten Sie mit anderen Gleam-Enthusiasten in Kontakt treten? Werden Sie Teil der Gleam-Community auf Discord: https://discord.com/invite/gleam*