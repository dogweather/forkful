---
title:                "Borttagning av tecken som matchar ett mönster"
html_title:           "Bash: Borttagning av tecken som matchar ett mönster"
simple_title:         "Borttagning av tecken som matchar ett mönster"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Om du står inför en situation där du behöver ta bort vissa tecken från en textsträng, kan det vara användbart att kunna använda en kommando i Bash för att lösa detta problem. Det kan till exempel vara att du vill ta bort alla mellanslag från en textfil för att göra den mer läsbar eller för att utföra andra manipulationer på den.

## Hur man gör
För att ta bort tecken som matchar ett visst mönster i Bash, kan du använda kommandot `sed`. Detta kommando kan användas för att söka efter en sträng av tecken och ersätta eller ta bort den.

Ett enkelt exempel på detta är om du har en textfil `exempel.txt` med innehåll som följer:

```Bash
Hej där! Följ med på en rolig resa!
```

Om du vill ta bort alla mellanslag från denna textfil kan du använda följande kommando:

```Bash
sed 's/ //g' exempel.txt
```

Resultatet kommer då att bli:

```Bash
Hejdär!Följmedpåenroligresa!
```

I detta exempel använde vi kommandot `sed` tillsammans med s-flaggan för att söka efter mellanslag (representarade som ` `) och ersätta dem med ingenting (representerat med `//`), så att de tas bort från den ursprungliga textfilen.

## Utforska djupare
Det finns flera andra sätt att använda kommandot `sed` för att ta bort tecken som matchar ett visst mönster. Du kan till exempel använda det för att ta bort tecken från specifika positioner i en textsträng eller för att ta bort vissa delar av en text. Det finns också andra kommandon i Bash som kan användas för att utföra liknande åtgärder, som till exempel `tr` och `awk`.

## Se även
- [Linux Command Library - sed](https://linuxcommandlibrary.com/man/sed.html)
- [Bash Hackers Wiki - Sed](https://wiki.bash-hackers.org/commands/classictest#stream_editing_with_sed)