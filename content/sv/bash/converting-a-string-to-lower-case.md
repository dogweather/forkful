---
title:                "Omvandla en sträng till gemener"
html_title:           "Bash: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver kan vara användbart för att standardisera indata, jämföra strängar utan att ta hänsyn till stora eller små bokstäver eller för att presentera data på ett enhetligt sätt.

## Så här gör du

För att konvertera en sträng till små bokstäver i Bash, kan du använda kommandot `tr`. Låt oss säga att vi har en sträng som heter "HELLO" och vill konvertera den till små bokstäver.

```Bash
my_string="HELLO"
echo $my_string | tr '[:upper:]' '[:lower:]'
```

Output:
```
hello
```

Här använder vi `tr`-kommandot tillsammans med dess mönsterfunktion för att konvertera alla stora bokstäver i strängen till motsvarande små bokstäver. Du kan också använda `sed` eller `awk` för liknande funktionalitet.

## Djupdykning

Bash erbjuder också inbyggda variabler som `^^` och `,,` för att konvertera enstaka tecken eller hela strängar till stora eller små bokstäver. Till exempel:

```Bash
my_string="WORLD"
echo ${my_string^^} #konverterar hela strängen till stora bokstäver
echo ${my_string,,} #konverterar hela strängen till små bokstäver
```

Output:
```
WORLD
world
```

En sak att vara medveten om är att dessa inbyggda operatorer endast fungerar i Bash version 4 eller högre.

## Se även

För mer information om hur du konverterar en sträng till små bokstäver i Bash, se följande länkar:

- [Bash Manual: "tr" kommandot](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-tr)
- [Bash Manual: Inbyggda variabler för att konvertera till stora och små bokstäver](https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html#index-_00242_00242-and-_002c_002c)