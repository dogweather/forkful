---
title:    "Bash: Omvandla en sträng till gemener"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till gemener (lower case) kan vara användbart när man behöver standardisera text eller sökningar, eller om man vill undvika potentiella fel på grund av skillnader mellan stora och små bokstäver.

## Så här gör man
För att konvertera en sträng till gemener i Bash kan man använda kommandot `tr` (translate) tillsammans med flaggan `-s` (squeeze) för att ta bort dubbletter och `-d` (delete) för att ta bort specifika tecken.

```Bash
# Exempel 1: Konvertera en hel sträng till gemener
$ echo "Hej alla programmerare!" | tr '[:upper:]' '[:lower:]'
hej alla programmerare!

# Exempel 2: Ta bort alla punkter och konvertera till gemener
$ echo "Välkommen till Bash-programmering." | tr -d '.' | tr '[:upper:]' '[:lower:]'
välkommen till bash-programmering
```

I den första raden använder vi `tr` för att konvertera alla stora bokstäver till gemener. I den andra raden använder vi först `tr` för att ta bort alla punkter och sedan en andra gång för att konvertera till gemener.

## Djupdykning
Det finns flera sätt att konvertera en sträng till gemener i Bash, men den enklaste metoden är att använda `tr`. Det finns många fler flaggor som kan användas, t.ex. `-c` för att komplementera och `-s` för att endast behålla ett exemplar av duplicerade tecken.

En annan teknik är att använda inbyggda Bash-kommandon som `printf` och `echo` tillsammans med parameterutbytning (`${parameter,,}`) för att konvertera till gemener.

```Bash
$ str="VÄLKOMMEN TILL BASH-PROGRAMMERING"
$ echo "${str,,}"
välkommen till bash-programmering
```

Det finns också mer avancerade metoder som använder regelbundna uttryck (regular expressions) eller tredjepartsverktyg som `sed` för att konvertera en sträng till gemener.

Det är viktigt att komma ihåg att vissa språk kan ha specifika regler för olika fall av gemener och att kommandot `tr` endast tar hänsyn till ASCII-tecken.

## Se även
- [Trigraph (Wikipedia)](https://en.wikipedia.org/wiki/Trigraph_(computing))
- [Bash Reference Manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- [Reguljära uttryck (Wikipedia)](https://sv.wikipedia.org/wiki/Regulj%C3%A4ra_uttryck)