---
title:                "Skriva till standardfel"
html_title:           "PHP: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att skriva till standard error kan vara ett användbart verktyg för att felsöka och hitta problem i din PHP-kod. Genom att skicka felmeddelanden till standard error istället för standard output, kan du enklare spåra och förstå varför vissa delar av din kod inte fungerar som den ska.

## Så här gör du
För att skriva till standard error i PHP, använd funktionen "fwrite" tillsammans med "STDERR" som filhandtag. Detta skickar dina meddelanden till standard error istället för standard output. Här är ett exempel på hur du kan använda det i din kod:

```PHP
fwrite(STDERR, "Detta är ett felmeddelande som skickas till standard error");
```
Output: Detta är ett felmeddelande som skickas till standard error

Detta kan vara särskilt användbart om du vill testa olika delar av din kod och se vad som orsakar problem. Genom att skriva ut felmeddelanden direkt till standard error, behöver du inte gå igenom hela koden för att hitta var felet uppstår.

## Djupdykning
Det finns flera olika sätt att skriva till standard error i PHP, men "fprintf" och "error_log" är två andra vanliga funktioner som kan användas för att skicka felmeddelanden. Du kan också använda "php://stderr" som ett filhandtag för att skriva direkt till standard error utan att använda "fwrite".

En viktig sak att tänka på när du skriver till standard error är att använda "die()" funktionen efter ditt felmeddelande för att avsluta programmet. Om du inte gör detta kan felmeddelandet försvinna bland andra utskrifter från programmet.

## Se även
- PHP-funktionen "fwrite": http://php.net/manual/en/function.fwrite.php
- PHP-funktionen "fprintf": http://php.net/manual/en/function.fprintf.php
- PHP-funktionen "error_log": http://php.net/manual/en/function.error-log.php
- Mer information om att skriva till standard error i PHP: http://php.net/manual/en/features.commandline.io-streams.php