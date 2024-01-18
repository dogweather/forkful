---
title:                "Das Schreiben von Tests"
html_title:           "Lua: Das Schreiben von Tests"
simple_title:         "Das Schreiben von Tests"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Writing Tests ist eine Methode, um die Funktionalität von Code zu überprüfen, insbesondere bei der Entwicklung von Software. Es ist wichtig, da es dabei hilft, Fehler zu finden und zu beheben, bevor der Code in Produktion geht.

## Wie geht's:
In Lua gibt es das Modul "luaunit", das Unterstützung für unit testing bietet. Es kann über Luarocks installiert werden:
```
luarocks install luaunit
```
Nach der Installation kann das Modul wie folgt in den Code eingebunden werden:
```
local luaunit = require('luaunit')
```
Hier ist ein einfaches Beispiel, das eine Funktion testet, die zwei Zahlen addiert:
```
function add(x, y)
    return x + y
end

TestAdd = {}

function TestAdd:test_add_positive_numbers()
    assertEquals(5, add(2, 3))
end
```
Ausführung des Tests:
```
> lua test.lua
```
Das Ergebnis sollte folgendermaßen lauten:
```
[================= TEST RESULTS =================]
[  RUN  ] TestAdd.test_add_positive_numbers
[  OK   ] TestAdd.test_add_positive_numbers
[================== TEST SUMMARY ==================
[  OK   ] 1 test / 0 failure / 0 error
```

## Tief tauchen:
Unit Testing ist eine gängige Praxis in der Softwareentwicklung und wird bereits seit den 1960er Jahren verwendet. Neben "luaunit" gibt es auch andere Frameworks wie "busted" und "luassert". Eine Alternative zum Unit Testing ist das manuelle Testen, was jedoch aufwendiger und fehleranfälliger sein kann. Bei der Implementierung von Tests ist es wichtig, sie regelmäßig auszuführen, um sicherzustellen, dass der Code weiterhin wie erwartet funktioniert.

## Siehe auch:
- [Luaunit Dokumentation](https://github.com/bluebird75/luaunit)
- [Busted Framework](https://olivinelabs.com/busted/)
- [Lua assert Dokumentation](http://www.lua.org/manual/5.3/manual.html#6.3)