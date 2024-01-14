---
title:    "PHP: Skriva en textfil"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför man ska skriva en textfil

Textfiler är en grundläggande del av programmering och används ofta för att lagra information eller konfigurationsdata. Att kunna skriva och läsa textfiler är en viktig färdighet för en PHP-utvecklare och kommer att hjälpa dig att bygga mer avancerade applikationer.

## Så här gör du

För att skriva en textfil i PHP, börjar vi med att öppna en fil för skrivning och tilldela den till en variabel. Sedan använder vi funktionen `fwrite()` för att skriva vår text till filen. När vi är klara skriver vi ut ett meddelande som bekräftar att filen skrivits.

En förenklad kod skulle se ut så här:

```PHP
$file = fopen("test.txt", "w");
fwrite($file, "Hej, det här är en textfil!");
fclose($file);

echo "Filen har skrivits!";
```

Denna kod öppnar en fil som heter "test.txt" och skriver sedan texten "Hej, det här är en textfil!" till filen. När vi är klara med att skriva stängs filen och vi får ett meddelande som bekräftar att filen har skrivits.

När vi öppnar vår textfil kan vi se att vår text har skrivits korrekt:

```
Hej, det här är en textfil!
```

Det är också möjligt att lägga till innehåll i en befintlig textfil utan att skriva över det som redan finns. Detta kan göras genom att använda läge "a" i `fopen()` istället för "w". Detta lägger till allt nytt innehåll i slutet av filen istället för att skriva över det som redan finns.

## Djupdykning

För att vara effektiv när du skriver textfiler i PHP är det viktigt att förstå de olika parametrarna som används i funktionerna `fopen()` och `fwrite()`. I exemplet ovan använde vi "w" som läge i `fopen()`. Detta står för "write" och tillåter oss att skriva till filen. Om vi skulle byta till läget "r" (read) skulle vi inte kunna skriva till filen, utan bara läsa från den.

När vi använder `fwrite()` är det viktigt att ta hänsyn till att vissa tecken kan vara skadliga för vår textfil. Om vi till exempel skulle använda citattecken eller andra specialtecken riskerar vi att skriva ogiltig data till filen. För att undvika detta kan vi använda funktionen `addcslashes()` för att escapera speciella tecken och se till att vår textfil inte blir korrupt.

Utöver detta finns det också många andra avancerade funktioner för att läsa och skriva textfiler i PHP, som till exempel `fgets()` och `file_put_contents()`. För att lära dig mer rekommenderar vi att du läser mer på PHP-språkets officiella dokumentation.

## Se även

- [PHP.net - Writing Files](https://www.php.net/manual/en/function.file-put-contents.php)
- [W3Schools - PHP File Handling](https://www.w3schools.com/php/php_file.asp)