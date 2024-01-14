---
title:    "Elm: Tests schreiben"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich überhaupt Gedanken um das Schreiben von Tests machen? Es gibt mehrere Gründe, warum es wichtig ist, beim Programmieren Tests zu integrieren. Zum einen sorgen Tests dafür, dass der Code funktioniert und mögliche Fehler frühzeitig erkannt werden, bevor sie in der Produktion auftreten. Außerdem helfen sie dabei, den Code zu strukturieren und zu verbessern. Durch das Schreiben von Tests wird man sich bewusster über den eigenen Code und kann somit eine bessere Qualität gewährleisten.

## Anleitung
Die Programmiersprache Elm bietet eine einfache und effiziente Möglichkeit, Tests zu schreiben. Im folgenden Beispiel werden wir eine einfache Funktion testen, die die Länge eines String berechnet.

```Elm
-- Beschreibung des Tests
elmTest "Calculate length of string" |> \_ ->
    -- Definieren eines String
    let
        input = "Hallo Welt"
    in
    -- Erwartete Ausgabe
    Expect.equal (String.length input) 10
```

In diesem Beispiel haben wir zuerst eine Beschreibung des Tests angegeben, um zu erklären, was genau getestet wird. Anschließend haben wir einen String definiert und die erwartete Ausgabe mit der Funktion `Expect.equal` festgelegt. Beim Ausführen des Tests sollte die Funktion die Länge des Strings berechnen und mit der angegebenen Ausgabe vergleichen. Falls beide Werte übereinstimmen, wird der Test bestanden.

## Tiefenschärfe
Beim Schreiben von Tests ist es wichtig, verschiedene Szenarien abzudecken und sicherzustellen, dass alle möglichen Fälle getestet werden. Elm bietet dafür verschiedene Funktionen, wie zum Beispiel `Expect.equal`, `Expect.notEqual` und `Expect.isTrue` für Vergleiche oder `Expect.throws` für das Testen von Fehlern. Auch die Verwendung von `describe` und `describeOnly` ermöglicht eine strukturierte Darstellung der Tests. Wenn man sich tiefer in das Thema einarbeiten möchte, empfehle ich die offizielle Dokumentation von Elm zur Testausführung.

## Siehe auch
- {Link zu einer Website mit weiteren Informationen zum Schreiben von Tests}
- {Link zu einem Video-Tutorial für das Schreiben von Tests in Elm}
- {Link zu einem GitHub-Repository mit Beispielen für Tests in Elm}