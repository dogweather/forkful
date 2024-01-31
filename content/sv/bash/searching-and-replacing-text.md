---
title:                "Sökning och ersättning av text"
date:                  2024-01-20T17:57:29.176776-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sökning och ersättning av text"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Sökning och ersättning av text hanterar automatisk ändring av strängar i filer. Programmerare använder det för att effektivisera uppdateringar, korrigera fel eller ändra kod snabbt.

## Hur gör man:
```Bash
# Sök och ersätt första förekomsten av "gammalt" med "nytt" i filen.txt
sed 's/gammalt/nytt/' filen.txt

# Sök och ersätt alla förekomster av "äpple" med "päron" i filen.txt
sed 's/äpple/päron/g' filen.txt

# Sök och ersätt text med backup av originalfilen
sed -i.bak 's/Windows/Linux/g' filen.txt

# Sök och ersätt i flera filer med loop
for f in *.txt; do
  sed -i 's/fotboll/ishockey/g' "$f"
done
```
Exempel utdata: `sed` kommandot ändrar texten i filerna och den modifierade texten visas om inte `-i` (in-place) flaggan används.

## Djupdykning:
Sök och ersätt funktionaliteten är grundläggande i Unix-liknande system och går tillbaka till tidiga textredigeringsverktyg som `ed` och `ex`. `sed`, som står för stream editor, blev en utvidgning av dessa verktyg med fokus på textflöden (pipes). Alternativ inkluderar moderna verktyg som `awk` för mer komplex bearbetning eller programmeringsspråk som Python för skriptning. I `sed`, står `s` för substitute (ersätt) och `g` för global (över hela filen). Användning av `-i` flaggan skapar en ny fil med ändringarna och kan kombineras med en extension (t.ex., `.bak`) för att spara en backup av originalfilen.

## Se också:
- [GNU sed manual](https://www.gnu.org/software/sed/manual/sed.html)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/index.html)
- [Stack Overflow: How do I use variables in a sed command?](https://stackoverflow.com/questions/6659351/how-do-i-use-variables-in-a-sed-command)
