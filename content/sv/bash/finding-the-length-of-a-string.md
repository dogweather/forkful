---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & varför?

Att hitta längden på en sträng innebär att ta reda på antalet tecken i strängen. Det är vanligt i programmering, bland annat för att undvika fel vid manipulation av strängdata, som att klippa strängar efter en exakt position.

## Hur man gör:

För att ta reda på längden på en sträng i Bash använder vi `${#string}` syntaxen. Här är en kodsnutt och utdatan den ger.

```Bash
sträng="Hej Sverige"
echo ${#sträng}
```

Det kommer att ge ut datan:

```Bash
12
```

## Djupdykning

Historiskt sett har funktionen för att hitta längden på en sträng alltid varit en del av Bash. Alternativen till `${#string}` syntaxen är begränsade i Bash. andra programmeringsspråk, som Python eller JavaScript, erbjuder funktioner som `.length()` eller `.len()` för att hitta längden på en sträng. 

Om du behöver hitta längden på en sträng i en äldre version av Bash, kan du använda `expr` kommandot så här:

```Bash
sträng="Hej Sverige"
längd=`expr length "$sträng"`
echo $längd
```

## Se även

1. [Bash String Operations](https://www.gnu.org/software/bash/manual/html_node/String-Operations.html): mer information om strängoperationer i Bash, inklusive att hitta stränglängder.
2. [Pure bash bible](https://github.com/dylanaraps/pure-bash-bible): en bok med tips om strikt bash-programmering.
3. [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/): en omfattande guide till bash-scripting som också täcker stränglängder.