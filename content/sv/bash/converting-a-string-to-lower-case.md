---
title:                "Konvertera en sträng till gemener"
date:                  2024-01-20T17:37:48.961237-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att ändra alla storbokstäver i texten till småbokstäver. Programmerare gör det för att förenkla jämförelse av texter och hantera användarinput som inte är fallet känslig.

## Gör så här:
```Bash
# Användning av tr-kommandot
echo "Hej Alla Där Ute!" | tr '[:upper:]' '[:lower:]'

# Användning av Bash inbyggda funktionalitet
str="Hej Alla Där Ute!"
echo "${str,,}"

# Sed-kommando för samma uppgift
echo "Hej Alla Där Ute!" | sed 's/[A-Z]/\L&/g'
```
Exempel på output:
```
hej alla där ute!
hej alla där ute!
hej alla där ute!
```

## Fördjupning:
Att konvertera till gemener är inte ett nytt koncept och funnits länge i olika programmeringsmiljöer och verktyg. I Bash-programmering har `tr` varit det traditionella verktyget för att omvandla text. Det fungerar som en strömredigerare som översätter eller raderar tecken.

`tr` kommandot är snabbt och effektivt men har begränsningar, särskilt när det gäller variabler. Därför introducerades i nyare versioner av Bash en inbyggd funktionalitet för strängmanipulering, som till exempel `${str,,}` för att omvandla alla bokstäver i en variabel `str` till gemener.

`sed` är ett annat kraftfullt verktyg för textbearbetning och kan också användas för denna uppgift, men det kan vara lite överdrivet för enkel textomvandling då det är tänkt för mer komplexa textmanipuleringar.

Det är viktigt att känna till att inte alla system kanske använder samma version av Bash eller har samma verktyg tillgängliga, och därför kan det vara bra att känna till alternativen.

## Se även:
- Bash manual: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- `tr` kommando: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- `sed` manual: https://www.gnu.org/software/sed/manual/sed.html
