---
title:                "String-Interpolation"
html_title:           "Lua: String-Interpolation"
simple_title:         "String-Interpolation"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Interpolation von Strings ist eine Technik, die von Programmierern verwendet wird, um innerhalb eines Strings variablen Inhalt einzufügen. Zum Beispiel könnte man einen Satz wie "Mein Name ist [Name]" interpolieren, wo [Name] durch den Namen des Benutzers ersetzt wird. Dies macht den Code flexibler und vereinfacht das Erstellen von dynamischen Strings.

## Wie geht's?

Die Interpolation von Strings kann in Lua mit dem %-Operator und zwei geschweiften Klammern durchgeführt werden. Zum Beispiel:

```Lua
local name = "Max"
print("Mein Name ist %{name}")
```

Dies würde die Ausgabe "Mein Name ist Max" erzeugen. Beachten Sie, dass der Variablenname innerhalb der geschweiften Klammern dem tatsächlichen Namen der Variable entsprechen muss.

Um mehrere Variablen zu interpolieren, können diese mit einem Komma getrennt innerhalb der geschweiften Klammern angegeben werden:

```Lua
local name = "Max"
local age = 25
print("%{name} ist %{age} Jahre alt.")
```

Dies würde die Ausgabe "Max ist 25 Jahre alt." erzeugen.

## Tiefentauchen

Die Methode der Interpolation von Strings ist nicht unbedingt neu. Andere Programmiersprachen wie Ruby, Python und Perl verwenden ähnliche Techniken, um Strings zu manipulieren. Alternativ können Sie auch die string.format()-Funktion in Lua verwenden, um Strings zu interpolieren. Diese Methode erfordert jedoch etwas mehr Code.

Implementierungsdetails: Der %-Operator in Lua ist ein Alias für die string.format()-Funktion. Die in den geschweiften Klammern angegebenen Variablen werden zu einer Tabelle zusammengeführt und als Argumente an die Funktion übergeben.

## Siehe auch

Weitere Informationen zur Interpolation von Strings in Lua finden Sie in der offiziellen Dokumentation: https://www.lua.org/manual/5.3/manual.html#6.4.1

Für Informationen zu anderen Möglichkeiten, Strings in Lua zu manipulieren, schauen Sie sich die String-Bibliothek an: https://www.lua.org/manual/5.3/manual.html#6.4