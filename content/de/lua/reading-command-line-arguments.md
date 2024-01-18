---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Lua: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Reading command line arguments (das Lesen von Befehlszeilenargumenten) is a way for programmers to pass data or information to their program when it is launched from the command line. This can be useful for providing initial settings or parameters for the program to use.

## Wie geht's?

Die Lua-Sprache bietet eine eingebaute Funktion, um Befehlszeilenargumente zu lesen. Diese Funktion heißt ```arg``` und gibt ein Array zurück, das alle übergebenen Argumente enthält. Hier ist ein Beispiel:

```
Lua myprogram.lua hello world
```
```
arg[0] == "myprogram.lua"
arg[1] == "hello"
arg[2] == "world"
```

Mit diesem Wissen können wir nun in unserem Programm auf die argumente zugreifen und sie verwenden. Zum Beispiel könnten wir einen grüßenden Satz ausgeben:

```lua
print("Hallo " .. arg[1] .. " und " .. arg[2] .. "!")
```

Die Ausgabe wäre dann: ```Hallo hello und world!```

## Tief eintauchen

Das Lesen von Befehlszeilenargumenten ist seit langem eine gängige Praxis in der Programmierung. Es kann eine praktische Möglichkeit sein, Optionen oder Einstellungen für ein Programm bereitzustellen, ohne diese manuell ändern zu müssen. Alternativ können auch Umgebungsvariablen verwendet werden, um Informationen an ein Programm zu übergeben.

In Lua können auch Parameter in der Form von Key-Value-Paaren angegeben werden, indem man sie mit einem Gleichheitszeichen trennt: ```Lua myprogram.lua --name=Max --age=25```

Die Parameter können dann in einem Table gespeichert werden, indem man die ```getopt```-Funktion verwendet.

## Siehe auch

- [Offizielle Lua Dokumentation](https://www.lua.org/manual/5.3/manual.html#6.10)
- [Das Lesen von Befehlszeilenargumenten in anderen Sprachen](https://stackoverflow.com/questions/14401844/how-to-read-command-line-arguments-in-a)