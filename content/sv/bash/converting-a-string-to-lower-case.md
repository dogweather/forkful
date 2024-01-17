---
title:                "Omvandla en sträng till små bokstäver"
html_title:           "Bash: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Konvertering av en sträng till gemener är en vanlig åtgärd inom programmering, där strängar (ord eller textbitar) ändras från versaler till gemener (stora till små bokstäver). Detta gör det enklare att jämföra och söka igenom strängar, samt ger en enhetlig formatering.

## Såhär gör du:
```Bash
#!/bin/bash

# Skapa en variabel med en sträng i versaler
STRANG="EXEMPEL TEXT"

# Använd kommandot 'tr' för att konvertera strängen till gemener
gemena_strang=$(echo "$STRANG" | tr '[:upper:]' '[:lower:]')

# Skriv ut den konverterade strängen
echo $gemena_strang
```

Output:
```
exempel text
```

## Djupdykning:
Att konvertera strängar till gemener har funnits i programmering sedan lång tid tillbaka och är en viktig komponent för hantering av textdata. En annan metod för konvertering är att använda kommandot 'awk', men 'tr' betraktas som mer effektivt. Implementationen av konverteringen kan variera beroende på operativsystem, men konceptet är detsamma.

## Se även:
- [Bash tr kommandodokumentation](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Alternativ för att konvertera strängar i Bash](https://stackoverflow.com/questions/2264428/how-to-convert-uppercase-to-lowercase-in-bash)