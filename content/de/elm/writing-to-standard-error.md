---
title:    "Elm: Schreiben nach Standardfehler"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben auf den Standardfehler (standard error) ist eine wichtige Fähigkeit beim Programmieren in Elm. Es ermöglicht es uns, Fehlermeldungen und andere wichtige Informationen auf der Konsole auszugeben, um unseren Code zu debuggen.

# Wie geht das?

Das Schreiben auf den Standardfehler in Elm ist einfach und erfordert nur einige Zeilen Code. Wir nutzen dafür die eingebaute `Debug` Bibliothek und die `log` Funktion. Schauen wir uns ein Beispiel an:

```Elm
import Debug

-- Eine Zahl eingeben
zahl = 5

-- Ausgabe der Zahl auf dem Standardfehler
Debug.log "Die Zahl ist:" zahl
```

Wenn wir dieses Programm ausführen, sehen wir die Ausgabe `Die Zahl ist: 5` auf unserer Konsole. Wie Sie sehen, haben wir die Funktion `Debug.log` genutzt, um einen Wert auf den Standardfehler zu schreiben. Diese Funktion erwartet zwei Argumente: einen String mit einer Beschreibung und den Wert, den wir ausgeben möchten.

# Tiefergehende Informationen

Manchmal wollen wir nicht nur einfache Werte, sondern komplexere Datenstrukturen auf den Standardfehler ausgeben. Dafür gibt es die Funktion `Debug.toString`, die ein beliebiges Elm-Objekt in einen String umwandelt. Wir können sie mit `Debug.log` kombinieren, um eine detailliertere Ausgabe zu erhalten:

```Elm
import Debug

-- Ein Datensatz definieren
type alias Person =
    { name : String
    , alter : Int
    }

-- Eine Person erstellen
person = Person "Max" 25

-- Ausgabe der Person auf dem Standardfehler
Debug.log "Die Person ist:" (Debug.toString person)
```

Die Ausgabe sieht nun folgendermaßen aus: `Die Person ist: { name = "Max", alter = 25 }`. Auf diese Weise können wir komplexere Datenstrukturen untersuchen, um Fehler in unserem Code zu finden.

# Siehe auch

- Offizielle Elm Debugging-Dokumentation: https://guide.elm-lang.org/debugging/debugging.html
- Mehr über die `Debug` Bibliothek erfahren: https://package.elm-lang.org/packages/elm/core/latest/Debug