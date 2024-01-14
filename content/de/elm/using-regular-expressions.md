---
title:    "Elm: Verwendung von regulären Ausdrücken"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum Sie sich für die Verwendung von regulären Ausdrücken (oder kurz Regex) in Elm entscheiden könnten. Vielleicht möchten Sie eine anspruchsvolle Validierungsfunktion für Benutzereingaben erstellen oder ein Muster in einem Text suchen. Egal aus welchem Grund, reguläre Ausdrücke sind ein leistungsstarkes Werkzeug, das Ihnen dabei helfen kann, komplexe Aufgaben in Elm zu lösen.

# Wie geht es?

Wenn Sie bereits mit regulären Ausdrücken in anderen Programmiersprachen vertraut sind, werden Sie feststellen, dass die Syntax in Elm sehr ähnlich ist. Um einen regulären Ausdruck in Elm zu schreiben, müssen Sie das Modul `Regex` importieren.

Schauen wir uns ein Beispiel an, um ein besseres Verständnis zu bekommen:

````Elm
import Regex exposing (..)

-- Prüft, ob eine Zeichenfolge eine gültige E-Mail-Adresse ist
isValidEmail : String -> Bool
isValidEmail email =
    regex (Regex.fromString "[a-z0-9._ %+-]+@[a-z0-9.-]+\\.[a-z]{2,}") email |> Result.isOk
````

In diesem Beispiel importieren wir das komplette `Regex`-Modul und definieren eine Funktion, die überprüft, ob eine Zeichenfolge eine gültige E-Mail-Adresse ist. Wir verwenden die `regex`-Funktion, die versucht, den regulären Ausdruck in der ersten Argumente auf die Zeichenfolge im zweiten Argument anzuwenden. Wenn die Übereinstimmung erfolgreich ist, gibt die Funktion `Ok` zurück, sonst `Err`.

# Tiefer ins Thema

Wie Sie im obigen Beispiel sehen können, besteht der reguläre Ausdruck aus verschiedenen Buchstaben, Zahlen und Sonderzeichen. Aber was bedeuten sie eigentlich?

Ein regulärer Ausdruck stellt ein Muster dar, nach dem ein Text gesucht oder validiert werden kann. In unserem Beispiel verwenden wir vier Klassen von Zeichen:

- `[a-z0-9._ %+-]`: Dies steht für alle Kleinbuchstaben, Zahlen sowie die Zeichen `._ %+-`.
- `@[a-z0-9.-]+`: Dies bedeutet, dass der Text `@` und dann eine oder mehrere Kleinbuchstaben, Zahlen oder das Zeichen `.` oder `-` enthalten muss.
- `\\.`: Das doppelte Backslash ist nötig, um das Sonderzeichen `.` zu "escapen", da es normalerweise für jedes beliebige Zeichen steht. Hier steht es aber ganz konkret für den Punkt.
- `{2,}`: Dies gibt an, dass das vorherige Element mindestens zwei Mal vorkommen muss. In unserem Fall muss die Domäne (z.B. `com`, `de`, `org`) mindestens zwei Buchstaben lang sein.

Mit regulären Ausdrücken können auch komplexere Muster definiert werden. Sie können beispielsweise auch Variablen und Gruppen verwenden, um bestimmte Teile eines Musters zu extrahieren. Für eine ausführlichere Erklärung empfehle ich, sich die offizielle [Dokumentation zu regulären Ausdrücken in Elm](https://package.elm-lang.org/packages/elm/regex/latest/) anzusehen.

# Siehe auch

- [Offizielle Dokumentation zu regulären Ausdrücken in Elm](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Online-RegEx-Tester](https://regexr.com/) zum Ausprobieren von regulären Ausdrücken
- [Reguläre Ausdrücke in der Praxis](https://medium.com/swlh/regex-in-practice-486ea9702657) - Ein hilfreicher Artikel, der verschiedene Anwendungsbeispiele für reguläre Ausdrücke gibt.