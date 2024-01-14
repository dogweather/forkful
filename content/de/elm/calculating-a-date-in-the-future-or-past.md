---
title:    "Elm: Berechnen eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit kann in Elm nützlich sein, um beispielsweise ein Datum für ein geplantes Event oder eine Deadline zu ermitteln.

# Wie

Die Berechnung eines Datums in der Zukunft oder Vergangenheit in Elm ist relativ einfach. Zunächst müssen wir eine Variable erstellen, die das aktuelle Datum repräsentiert:

```Elm
let
    today = Date.today
```

Anschließend können wir diese Variable mithilfe der Elm-Date-Library um die gewünschte Anzahl an Tagen erhöhen oder verringern. Zum Beispiel, um das Datum in 5 Tagen zu berechnen, können wir Folgendes tun:

```Elm
let
    today = Date.today
    futureDate = Date.inDays 5 today
```

Das Gleiche funktioniert auch für Daten in der Vergangenheit, indem wir die Anzahl der Tage negativ angeben. Zum Beispiel, um das Datum vor 10 Tagen zu berechnen:

```Elm
let
    today = Date.today
    pastDate = Date.inDays (-10) today
```

Um das Ergebnis in einem bestimmten Format auszugeben, können wir die `toString` Funktion verwenden und ein `Date.Format` Objekt angeben. Zum Beispiel, um das Datum in deutschem Format auszugeben:

```Elm
let
    today = Date.today
    futureDate = Date.inDays 5 today
    formattedDate = Date.format "dd.MM.yyyy" futureDate
in
    formattedDate -- Ergebnis: "25.05.2021"
```

# Tiefentauchen

Die Elm-Date-Library bietet noch viele weitere Funktionen zur Berechnung von Datum und Zeit. Wir können nicht nur Tage, sondern auch Stunden, Minuten, Monate, sowie Zeitzone- und Sommerzeit-Unterstützung angeben. Weitere Informationen zu diesen Funktionen können in der Dokumentation der Elm-Date-Library gefunden werden.

Weitere Erläuterungen und Beispiele zur Berechnung von Datum und Zeit in Elm können auch in der offiziellen Elm-Community gefunden werden.

# Siehe auch
- Elm-Date-Library Dokumentation: https://package.elm-lang.org/packages/elm/time/latest/
- Elm-Community: https://elm-lang.org/community