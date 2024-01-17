---
title:                "Radera tecken som matchar ett mönster"
html_title:           "Fish Shell: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort tecken som matchar ett visst mönster är en vanlig uppgift inom programmering. Det kan åstadkommas med hjälp av kommandot "delete" i Fish Shell. Detta är ett kraftfullt verktyg som gör det möjligt för utvecklare att effektivt hantera strängar och manipulera data i sina skript. 

## Så här gör man:
### Exempel 1: 
```
Fish Shell version 3.2.2
$ string='123abc456def'
$ set -l new_string
$ set -l pattern '[0-9]'
$ while string; set -l character; echo "string = '$string', character = '$character', new_string = '$new_string'"
  if string; string -e 's/'$pattern'//'; set new_string $new_string$character
```
```
string = "123abc456def", character = "1", new_string = ""
string = "23abc456def", character = "2", new_string = "1"
string = "3abc456def", character = "3", new_string = "12"
string = "abc456def", character = "a", new_string = "123"
string = "bc456def", character = "b", new_string = "123a"
string = "c456def", character = "c", new_string = "123ab"
string = "456def", character = "4", new_string = "123abc"
string = "56def", character = "5", new_string = "123abc4"
string = "6def", character = "6", new_string = "123abc45"
string = "def", character = "d", new_string = "123abc456"
string = "ef", character = "e", new_string = "123abc456d"
string = "f", character = "f", new_string = "123abc456de"
string = "", character = "", new_string = "123abc456def"
```
### Exempel 2:
```
Fish Shell version 3.2.2
$ string='Jan, Feb, Mar, Apr, May'
$ string -e 's/,//' 
Jan Feb Mar Apr May
```
I det första exemplet ser vi hur kommandot "string" används för att ta bort alla tecken som matchar mönstret "[0-9]", vilket i detta fall är alla siffror. I det andra exemplet ser vi hur en specifik karaktär, i det här fallet kommatecknet, tas bort med hjälp av "s/" kommandot. 

## Djupdykning:
"Delete" kommandot för Fish Shell har funnits sedan version 1.23 och har sedan dess varit en viktig del av både grundläggande och mer avancerade programmering. Alternativ för att ta bort tecken som matchar ett visst mönster finns också inom andra Unix-baserade skal, såsom Bash, men Fish Shell gör det enklare att läsa och skriva de nödvändiga kommandona. 

När det gäller implementationen av kommandot, så använder Fish Shell sig av en vanlig regelbunden uttryckssyntax (regex), som är en standard för att söka efter mönster inom strängar. Detta gör det enkelt att anpassa kommandot till olika användningsområden.

## Se även:
- [Fish Shell dokumentation](https://fishshell.com/docs/current/) 
- [Reguljära uttryck (Regex) guiden](https://developer.mozilla.org/sv/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Bash kommandon för att ta bort tecken](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)