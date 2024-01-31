---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"

category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva tester innebär att man skapar skript som automatiskt kontrollerar att kod gör det den ska. Programmerare gör detta för att hitta buggar snabbt, förbättra kodkvaliteten och säkerställa att framtida förändringar inte förstör befintlig funktionalitet.

## How to:
Skriv ett testscenarium för ett Bash-skript. Du vill verifiera att en funktion, `adder`, korrekt adderar två tal.

```Bash
#!/bin/bash

adder() {
    echo $(($1 + $2))
}

# Testfunktion
test_adder() {
    local result=$(adder 3 5)
    if [ $result -eq 8 ]; then
        echo "Test passed: 3 + 5 = 8"
    else
        echo "Test failed: Expected 8 but got $result"
    fi
}

# Kör testfunktionen
test_adder
```

Förväntad utskrift efter körning:

```
Test passed: 3 + 5 = 8
```

## Deep Dive:
Testning i Bash kan vara enkelt som ovan men det finns fler sofistikerade verktyg som shunit2 eller BATS (Bash Automated Testing System) för mer avancerade behov. Historiskt sätt har Bash inte haft samma fokus på testning som högre programmeringsspråk men detta har förändrats då skripten blivit mer komplexa. Alternativen varierar från enkla assert-baserade tester till ramverk som erbjuder mockning och stubbar.

## See Also:
- BATS: https://github.com/bats-core/bats-core
- shunit2: https://github.com/kward/shunit2
- Bash scripting guide: https://www.gnu.org/software/bash/manual/
