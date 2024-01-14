---
title:                "Javascript: Skriva till standardfel"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Varför skriva till standard error i Javascript

Att skriva till standard error i Javascript är ett bra sätt att felsöka och hantera eventuella fel som kan uppstå i din kod. Genom att läsa utskrifter i standard error kan du enkelt spåra vilka delar av din kod som kan orsaka problem.

## Så här gör du det

Först och främst måste du öppna standard error-strömmen med hjälp av console.error() funktionen. Du kan sedan skicka felmeddelanden eller utskrifter till standard error genom att först konvertera dem till en sträng och sedan skicka dem till console.error().

```Javascript
const error = "Ett fel uppstod!";
console.error(error); //skickar "Ett fel uppstod!" till standard error
```

## Djupdykning

Att skriva till standard error är ett sätt att skilja mellan återgivningen av vanliga utskrifter och felmeddelanden eller varningar. Genom att skicka felmeddelanden till standard error kan du tydligt se vad som är ett misstag och vad som är avsett att visas för användaren.

Det finns också ett alternativ till att använda console.error(), vilket är att använda console.log() och skicka med ett andra argument som indikerar att utskriften är ett felmeddelande. Till exempel:

```Javascript
const error = "Ett fel uppstod!";
console.log(error, {type: "error"}); //skickar "Ett fel uppstod!" till standard error
```

Detta kan vara användbart om du vill ha mer kontroll över hur ditt felmeddelande visas i konsolen.

# Se även

- [console.error() dokumentation](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [Skriva till standard error i Javascript](https://stackabuse.com/writing-to-standard-error-in-javascript/)