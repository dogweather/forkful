---
title:                "Skriving av tester"
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Tester verifiserer kode fungerer som forventet. Programmerere skriver tester for å fange feil tidlig, sikre kvalitet, og forenkle vedlikehold.

## How to:
Her er en enkel Bash-funksjon og en test for den i `test.sh`:
```Bash
# fil: functions.sh
function add_numbers {
    echo $(($1 + $2))
}

# fil: test.sh
source ./functions.sh

result=$(add_numbers 3 5)
if [ "$result" -eq 8 ]; then
    echo "Test passed!"
else
    echo "Test failed: expected 8 but got $result"
fi
```
Output etter kjøring av `bash test.sh`:
```
Test passed!
```

## Deep Dive
Bash er ikke primært for testing, men `test`-kommandoen og dens alias `[...]` tillater enkle sjekker. Historisk sett har Bash vokst fra en enkel kommandolinjetolker til et skriptspråk med logiske operasjoner og funksjoner. Andre løsninger som `shunit2` eller `BATS` gir rikere testrammeverk. Når du skriver tester i Bash, bruk funksjoner for å organisere kode og `source` for å inkludere.

## See Also
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)
- [shunit2 på GitHub](https://github.com/kward/shunit2) 
- [BATS på GitHub](https://github.com/sstephenson/bats)