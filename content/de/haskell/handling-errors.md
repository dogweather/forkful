---
title:                "Fehlerbehandlung"
date:                  2024-01-26T00:53:20.320524-07:00
model:                 gpt-4-1106-preview
simple_title:         "Fehlerbehandlung"

category:             "Haskell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/handling-errors.md"
---

{{< edit_this_page >}}

## Was & Warum?
Fehlerbehandlung in der Programmierung handelt davon, mit Unerwartetem umzugehen—Dinge, die schiefgehen können. Programmierer machen dies, um sicherzustellen, dass ihre Programme diese Situationen anmutig bewältigen können, ohne abzustürzen oder falsche Ergebnisse zu liefern.

## Wie:
Haskell behandelt Fehler robust durch Typen wie `Maybe` und `Either`. Hier ein kurzer Überblick:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Teilen durch null geht nicht, also geben wir Nothing zurück.
safeDivide x y = Just (x `div` y)  -- Ansonsten ist alles gut, wir geben das Ergebnis in einem Just zurück.

-- Lass es uns in Aktion sehen:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

Für komplexere Fehlerbehandlung kommt `Either` ins Spiel:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Fehler: Division durch Null."  -- Diesmal trägt der Fehler eine Nachricht.
safeDivideEither x y = Right (x `div` y)

-- Und in Verwendung:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Fehler: Division durch Null."
```

## Tiefgang
In der Haskell-Welt hat die Fehlerbehandlung eine lange Geschichte. Früher konnten Fehler das gesamte Programm zum Absturz bringen—kein Spaß. Das Typensystem von Haskell bietet Wege, um dies viel unwahrscheinlicher zu machen. Wir haben `Maybe` und `Either`, aber es gibt auch andere wie `Exceptions` und `IO` für verschiedene Szenarien.

`Maybe` ist einfach: Du erhältst `Just` etwas, wenn alles in Ordnung ist, oder `Nothing`, wenn es das nicht ist. `Either` geht einen Schritt weiter und ermöglicht dir, eine Fehlermeldung (`Left`) oder ein erfolgreiches Ergebnis (`Right`) zurückzugeben.

Beide sind rein, was bedeutet, dass sie die Außenwelt nicht beeinflussen – ein großes Ding in Haskell. Wir vermeiden die Fallstricke von ungeprüften Ausnahmen, die einige andere Sprachen plagen.

Für diejenigen, die mit `Maybe` und `Either` nicht zufrieden sind, bieten Bibliotheken wie `Control.Exception` traditionellere, imperativ-stilisierte Fehlerbehandlung durch Ausnahmen. Aber deren zu freizügige Verwendung kann die Dinge verkomplizieren, daher bleibt die Gemeinschaft oft bei den Typen.

## Siehe Auch
Tauche tiefer ein mit:

- Haskells eigenen Dokumenten: [Haskell](https://haskell.org/documentation)
- Ideal für Anfänger: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
