---
title:                "Zahlen runden"
date:                  2024-01-26T03:44:34.999038-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zahlen runden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zahlen zu runden bedeutet, einen Wert auf die nächstgelegene angegebene Stelle zu korrigieren – wie 2,56 zu 3, wenn wir auf ganze Zahlen runden. Programmierer tun dies aus Gründen der Einfachheit oder um bestimmte numerische Spezifikationen zu erfüllen, üblicherweise, um Nuancen zu vermeiden, die durch Genauigkeitsfehler bei Fließkommazahlen verursacht werden, oder um die Ausgabe benutzerfreundlich zu gestalten.

## Wie geht das:
In Gleam ist das Runden bis zu meinem letzten Check nicht in der Standardbibliothek enthalten, aber so würdest du typischerweise eine Fließkommazahl auf die nächste ganze Zahl runden, indem du direkt Erlang-Funktionen benutzt:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Gibt aus: 3
}
```

Ausgabe:
```
3
```

Eine andere Präzision im Sinn? Sagen wir, auf zwei Dezimalstellen runden? Wir brauchen ein bisschen Mathematik:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Gibt aus: 2.57
}
```

Ausgabe:
```
2.57
```

## Vertiefung
Historisch gesehen war das Runden von Zahlen besonders in der Finanz- und Wissenschaftsrechnung, wo Präzision und Normen enorm wichtig sind, entscheidend. Ohne Rundung würden überall unangenehme lange Dezimalzahlen auftreten, was Berechnungen unpraktisch und fehleranfällig machen würde.

In der Programmierwelt bieten verschiedene Sprachen unterschiedliche Ansätze, von integrierten Funktionen bis hin zu umfassenden Mathematikbibliotheken. Das Runden könnte verschiedene Regeln beinhalten - zum Beispiel "gerundet auf halbe auf" (die übliche Methode) oder "gerundet auf halb gerade" (oft verwendet in finanziellen Berechnungen, um Verzerrungen zu vermeiden).

Gleam, eine junge Sprache mit Wurzeln in Erlang, stützt sich auf Erlangs robusten Satz numerischer Funktionen. Mit dem Wachstum der Sprache könnten wir native Funktionen sehen, die eingeführt werden, und die Notwendigkeit, externe Routinen aufzurufen, reduzieren.

## Siehe auch
- Erlangs :math Modul für mehr Zahlenknacken: https://erlang.org/doc/man/math.html
- Für Hintergrundinformationen, warum das Runden knifflig sein kann, der IEEE-Standard für Fließkommazahlen: https://ieeexplore.ieee.org/document/8766229
- Interessiert an der Mathematik dahinter? Sieh dir an "Was jeder Informatiker über Fließkommarechnung wissen sollte": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
