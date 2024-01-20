---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hämta det nuvarande datumet är en viktig funktion som ger programmerare möjligheten att ändra beteende baserat på när ett skript eller kommando körs. Det är användbart för att logga, tidstämpla filer, och schemalägga händelser.

## Hur man gör det:

Hämta det nuvarande datumet i Fish Shell är lätt som en plätt. Använd bara kommandot `date`.

```Fish Shell
> date
Onsdag 9 mars 2022 kl. 16:00:00 CET
```
## Djupdykning

Det att hämta datum har historiskt sett alltid varit en grundläggande komponent i programmering. I Unix-baserade system, som Fish Shell, används `date` kommandot. Det är ett kraftfullt verktyg, vilket kan skräddarsys för att passa en mängd olika behov.

Om du behöver mer kraft från ditt datumkommando, överväg att använda `strftime` funktionen i Fish. Det låter dig ange en anpassad formatsträng för att få exakt den output du behöver.

```Fish Shell
> date "+%A %d %B %Y kl. %H:%M:%S %Z"
Onsdag 09 Mars 2022 kl. 16:00:00 CET
```
I vissa fall kan du använda alternativa sätt för att hämta datumet, som `clock_gettime` funktionen i C++. Detta kan ge mer precision, men det kan också vara mer komplicerat att använda.

## Se också:

För mer information om att använda datumkommandot och andra relaterade ämnen, kolla in följande länkar:

1. Fish Shell dokumentation: [Fish Shell](https://fishshell.com/docs/current/index.html)
2. GNU Core Utilities manual: [Date-command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
3. Strftime manuella sidan: [Strftime](https://man7.org/linux/man-pages/man3/strftime.3.html)
4. C++ dokumentation: [clock_gettime](http://www.cplusplus.com/reference/ctime/clock_gettime/)