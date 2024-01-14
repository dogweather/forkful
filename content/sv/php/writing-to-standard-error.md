---
title:                "PHP: Skriva till standardfel"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error i PHP är ett viktigt koncept som många nybörjare kanske inte förstår direkt. Detta blogginlägg kommer att gå igenom varför det är viktigt att kunna skriva till standard error och hur man gör det på rätt sätt.

## Hur man gör

Att skriva till standard error i PHP är relativt enkelt. För att skriva ett felmeddelande till standard error kan du använda funktionen "fwrite" tillsammans med standard error-konstanten "STDERR", som ser ut så här: 

```PHP
fwrite(STDERR, "Detta är ett felmeddelande!");
```

Detta kommer att skriva ut meddelandet "Detta är ett felmeddelande!" till standard error. Om du vill läsa mer om standard error-konstanten kan du läsa dokumentationen [här](https://www.php.net/manual/en/wrappers.php.php).

## Djupdykning

Ett vanligt misstag som nybörjare gör när de skriver till standard error är att blanda ihop det med standard output. Det är viktigt att förstå skillnaden mellan de två. Standard output används för att skriva ut information som användaren behöver se, medan standard error används för att skriva ut felmeddelanden som systemet behöver se.

En annan viktig sak att tänka på är att stänga standard error efter att du är klar med att skriva till det. Du kan göra detta genom att använda funktionen "fclose" tillsammans med standard error-konstanten "STDERR". Detta förhindrar läckor och håller din kod ren.

## Se även

 - [PHP manual för fwrite](https://www.php.net/manual/en/function.fwrite.php)
 - [PHP manual för fclose](https://www.php.net/manual/en/function.fclose.php)
 - [PHP manual för standard error-konstanten](https://www.php.net/manual/en/wrappers.php.php)