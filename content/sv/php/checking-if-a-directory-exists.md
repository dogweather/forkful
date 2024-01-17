---
title:                "Kontrollera om en mapp finns"
html_title:           "PHP: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Checka om en mapp finns är en viktig del av programmering eftersom det tillåter programmet att göra vissa åtgärder beroende på om en viss mapp finns eller inte. Detta hjälper till att göra koden mer robust och kraschsäker.

## Hur du gör:
Kolla om en mapp finns i PHP är enkelt tack vare den inbyggda funktionen `file_exists()`. Här är ett kodexempel:

```PHP
if (file_exists("min_mapp")) {
    echo "Mappen finns";
} else {
    echo "Mappen finns inte";
}
```

Detta kodexempel kommer att kolla om en mapp med namnet "min_mapp" finns och skriva ut ett lämpligt meddelande baserat på resultatet. Om mappen finns, kommer det att skriva ut "Mappen finns", annars kommer det att skriva ut "Mappen finns inte".

## Djupdykning:
Att kolla om en mapp finns är en viktig del av fil- och mapphantering i PHP. Innan inbyggda funktioner som `file_exists()` fanns, var det nödvändigt att använda mer komplexa metoder som att köra systemanrop för att utföra samma uppgift. Nu kan vi enkelt använda den inbyggda metoden som gör att koden blir mer effektiv.

Om du letar efter en mer specifik metod för att kolla om en mapp finns, kan du också använda `is_dir()` som returnerar sant om sökvägen leder till en mapp. Detta kan vara användbart om du bara vill kontrollera om en specifik sökväg pekar på en mapp eller inte.

## Se också:
- [PHP manual för file_exists()](https://www.php.net/manual/en/function.file-exists.php)
- [PHP manual för is_dir()](https://www.php.net/manual/en/function.is-dir.php)