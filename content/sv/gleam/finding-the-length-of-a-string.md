---
title:    "Gleam: Att hitta längden på en sträng."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

### Varför
Att hitta längden på en sträng är en grundläggande funktion inom programmering och är ofta användbar för att manipulera text och data. Genom att lära sig hur man gör det kan du enkelt hantera och bearbeta text i dina applikationer.

### Så här gör du
Att hitta längden på en sträng är en enkel process i Gleam. Använd funktionen `length` för att få längden på en sträng och spara den till en variabel.

```Gleam
let sträng = "Hej från Gleam!"
let längd = length(sträng)
```

`längd` variabeln kommer nu att innehålla värdet `14`, vilket representerar antalet tecken i strängen.

### Deep Dive
Att förstå hur längden på en sträng beräknas är viktigt för att kunna använda denna funktion på ett effektivt sätt. I Gleam, liksom i många andra programmeringsspråk, räknas varje tecken som en enhet i strängen. Det betyder att alla tecken, inklusive mellanslag och specialtecken, räknas med i längden.

Du kan också använda `length` funktionen på andra typer av data, såsom listor och tupler, för att få antalet element i dem.

### Se också
- [Officiell Gleam-dokumentation för längd funktionen](https://gleam.run/documentation/std/string#length)
- [En översikt över Gleam-programmeringsspråket](https://gleam.run/)
- [Exempelkod för att hitta längden på en sträng i Gleam](https://gist.github.com/example-code)