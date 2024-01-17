---
title:                "Skriva till standard error"
html_title:           "Bash: Skriva till standard error"
simple_title:         "Skriva till standard error"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?

När vi programmerar i Bash, kan vi ibland stöta på fel eller varningar som vi vill kommunicera till användaren. För att göra detta, använder vi "standard error", också kallad "stderr". Detta är en ström där vi kan skicka meddelanden till användaren som är separat från "standard output", eller "stdout". Detta är användbart när vi vill skilja mellan olika typer av utdata i vårt program.

## Hur du gör:

För att skriva till standard error i Bash, kan vi använda kommandot "echo" tillsammans med ">&2". Detta leder all utdata från echo till stderr istället för stdout. Vi kan också använda "&>" för att skriva både till stdout och stderr samtidigt.

Exempel:

```Bash
echo "Felmeddelande" >&2
```
Output:
```
Felmeddelande
```

Vi kan också använda ">>" för att lägga till text istället för att skriva över det som redan finns. Till exempel:

```Bash
echo "Varning!" >>&2
```
Output:
```
Varning!
```

## Djupdykning:

Att skriva till standard error är en vanlig teknik som används av många programmerare. Det är speciellt användbart när vi vill hålla isär utdata från fel och utdata från vårt program. Innan standard error introducerades, var det vanligt att skriva alla meddelanden till stdout och separera dem med specialtecken. Men detta gjorde det svårt att skilja mellan utdata och felmeddelanden.

Som alternativ till att skriva till standard error, kan vi också använda loggfiler för att spåra felmeddelanden. Men detta kan kräva mer ansträngning att implementera och skulle inte fungera för ett interaktivt program. Det är också viktigt att tänka på att skriva till stderr kan påverka prestandan för vårt program, så det är viktigt att inte överanvända det.

## Se även:

Om du vill lära dig mer om att skriva till standard error, kan du titta på följande länkar för mer information:

- [Bash dokumentation om stderr](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [En översikt av Bash stderr och stdout](https://www.howtoforge.com/tutorial/working-with-standard-output-and-error-output/)
- [En diskussion om skillnaderna mellan stdout och stderr](https://unix.stackexchange.com/questions/157133/what-is-the-difference-between-stdout-and-stderr)