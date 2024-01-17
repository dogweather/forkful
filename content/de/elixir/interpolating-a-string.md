---
title:                "String-Interpolation"
html_title:           "Elixir: String-Interpolation"
simple_title:         "String-Interpolation"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Interpolation in Elixir ist eine Möglichkeit, Werte oder Variablen in einen String einzubetten, um die Lesbarkeit und Ästhetik des Codes zu verbessern. Durch die Verwendung von Interpolation können wir effizienter auf Daten zugreifen, anstatt lange Zeichenketten manuell zu verbinden. Dies macht es zu einer beliebten Technik unter Programmierern, die sauberen und kompakten Code schätzen.

## Wie geht's?

Interpolation in Elixir wird mithilfe eines speziellen Operators, dem `#{}` Operator, durchgeführt. Um Werte in einen String zu interpolieren, müssen diese in den `#{}`-Operator eingebettet werden. Im Folgenden sehen Sie ein einfaches Beispiel:

```elixir
name = "Max"
"Hello #{name}!" # Output: Hello Max!
```

Natürlich können nicht nur Variablen, sondern auch Ausdrücke und Funktionen in den `#{}`-Operator eingebettet werden. Ein weiteres Beispiel:

```elixir
age = 25
"Max is #{age + 5} years old." # Output: Max is 30 years old.
```

## Tiefere Einblicke

Interpolation ist nicht nur in Elixir, sondern auch in anderen Programmiersprachen wie Ruby und JavaScript üblich. Es ist auch eine praktische Alternative zum manuellen Verbinden von Zeichenketten mit dem `<>`-Operator. Abgesehen davon spielt Interpolation eine wichtige Rolle in der funktionalen Programmierung, da sie es ermöglicht, Variablen und Funktionen auf eine elegante Weise in Strings einzubetten.

Die Implementierung von Interpolation in Elixir ist effizienter als in anderen Sprachen, da sie intern den `Kernel`-Modul verwendet. Dies bedeutet, dass jede Funktion, die in Interpolation verwendet wird, automatisch aufgerufen wird, wodurch der Code sauberer und lesbarer wird.

## Siehe auch

Weitere Informationen zu Interpolation in Elixir finden Sie in der offiziellen Dokumentation unter [https://elixir-lang.org/getting-started/interpolation.html](https://elixir-lang.org/getting-started/interpolation.html). Wenn Sie sich tiefer mit dem Thema befassen möchten, werfen Sie einen Blick auf die offizielle Elixir-Community-Website unter [https://elixir-lang.org/](https://elixir-lang.org/).