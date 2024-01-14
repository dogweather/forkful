---
title:                "PHP: Att ladda ner en webbsida"
simple_title:         "Att ladda ner en webbsida"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Varför: Det är vanligtvis nödvändigt att ladda ner en webbsida när du behöver visa information från en extern källa, såsom att integrera en integrerad social media-stream på din sida eller hämta data från en API.

Hur man gör: För att ladda ner en webbsida i PHP behöver du använda "file_get_contents" -funktionen och ange URL:en till den webbsida du vill hämta. Du kan sedan använda PHPs inbyggda funktioner för att manipulera och visa informationen. Här är ett exempel på kod för att hämta en sida och visa dess innehåll:

```PHP
<?php
// Hämtar webbsidan
$webbsida = file_get_contents('https://www.example.com');

// Visar innehållet på sidan
echo $webbsida;
?>
```

Detta kommer att skriva ut hela webbsidan i din kod, vilket kan vara användbart för testning och felsökning. Du kan också använda olika inbyggda funktioner som "substr" och "strpos" för att manipulera och filtrera den hämtade informationen.

Djupdykning: När du hämtar en webbsida i PHP, hämtar du faktiskt bara dess HTML-kod. Detta innebär att om webbsidan innehåller dynamiskt genererad information, såsom med JavaScript eller server-side scripts, kommer du inte att kunna hämta den med denna metod. I så fall måste du använda en kombination av PHP och JavaScript för att hämta och visa den dynamiska informationen.

Se även: För mer information om att hämta webbsidor i PHP, kolla in dessa resurser:

- [PHP.net - file_get_contents](https://www.php.net/manual/en/function.file-get-contents.php)
- [W3Schools - PHP file_get_contents](https://www.w3schools.com/php/func_filesystem_file_get_contents.asp)
- [Garage Web Guru - PHP: Hur man hämtar en webbsida](https://garagewebsite.com/parts/web-page:php/how-to-download-a-web-page/)

Vi hoppas att denna guide har hjälpt dig att förstå hur du kan ladda ner en webbsida i PHP. Tack för att du läste och lycka till med din kodning!