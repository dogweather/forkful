---
title:    "Elm: Zufallszahlen generieren"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Warum

Sich mit der Generierung von Zufallszahlen zu beschäftigen, kann ein nützliches Werkzeug sein, um Spiele, Simulationen oder andere dynamische Anwendungen in Elm zu erstellen. Durch die Integration von Zufallszahlen in Ihr Programm können Sie eine unberechenbare und unterhaltsame Komponente hinzufügen.

## Wie

Die Generierung von Zufallszahlen in Elm ist recht einfach und kann mithilfe der "Random" Bibliothek durchgeführt werden. Hier ist ein Beispiel, wie Sie eine Zufallszahl zwischen 1 und 10 generieren können:

```Elm
import Random exposing (between)
import Time exposing (millisecond)

-- Generiert eine Zufallszahl zwischen 1 und 10 basierend auf der aktuellen Zeit in Millisekunden
randomNumber : Time.Posix -> Int
randomNumber time =
    between 1 10 (millisecond time)

-- Ausgabe: 8
randomNumber 1546732800000
```

Sie können auch mehrere Zufallszahlen auf einmal generieren oder die Generierung von Zufallszahlen von einem bestimmten Seed abhängig machen. Weitere Informationen dazu finden Sie in der Dokumentation der "Random" Bibliothek.

## Tieferer Einblick

Wenn Sie tiefer in die Generierung von Zufallszahlen einsteigen möchten, sollten Sie sich die "System.Random" Bibliothek anschauen. Diese Bibliothek bietet zusätzliche Funktionen und einen größeren Bereich von generierten Zufallszahlen im Vergleich zur "Random" Bibliothek.

Sie können auch benutzerdefinierte Generatoren erstellen, um komplexe Zufallszahlen zu generieren. Dies kann hilfreich sein, wenn Sie spezifische Anforderungen an Ihre Zufallszahlen haben. Eine detaillierte Anleitung dazu finden Sie in der "System.Random" Dokumentation.

## Siehe auch

- Die offizielle "Random" Bibliothek Dokumentation: https://package.elm-lang.org/packages/elm/random/latest/
- Die "System.Random" Bibliothek Dokumentation: https://package.elm-lang.org/packages/elm/random/latest/System-Random