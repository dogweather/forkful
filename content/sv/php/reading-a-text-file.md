---
title:    "PHP: Läsning av en textfil"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Varför
Att kunna läsa textfiler är en viktig färdighet inom PHP-programmering. Det tillåter dig att hämta information från externa källor och använda den i dina program. Detta kan vara användbart för att skapa dynamiska webbsidor eller för att utföra olika uppgifter på din server.

# Hur du gör det
För att läsa en textfil i PHP, kan du använda funktionen "file_get_contents" tillsammans med "echo" för att skriva ut innehållet på filen. Till exempel:

```PHP
$innehall = file_get_contents("filnamn.txt");
echo $innehall;
```

Detta kommer att hämta allt innehåll från filen "filnamn.txt" och skriva ut det på skärmen. Du kan också använda funktionen "file" för att läsa filen rad för rad, vilket kan vara användbart om du vill manipulera datan innan du skriver ut den.

```PHP
$fil = fopen("filnamn.txt", "r");
while (!feof($fil)) {
    echo fgets($fil);
}
fclose($fil);
```

Detta kodblock öppnar filen "filnamn.txt" för läsning med funktionen "fopen", och sedan använder en "while"-loop tillsammans med "fgets" för att skriva ut varje rad tills filen är slut. Till sist stängs filen med "fclose".

# Djupdykning
När du läser en textfil i PHP, är det viktigt att ha koll på filens sökväg och filens rättigheter. Om filen inte finns på den angivna sökvägen eller om den inte har rätt behörigheter, kommer inte läsningen att lyckas.

Det är också bra att använda funktionen "file_exists" för att kontrollera om filen faktiskt finns innan du försöker läsa den. Om du vill manipulera datan som lästs från filen, kan du använda funktionen "explode" tillsammans med en separator (till exempel ett kommatecken) för att dela upp datan i olika variabler eller arrayer.

# Se också
- [PHP manual: file_get_contents](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP manual: file](https://www.php.net/manual/en/function.file.php)
- [PHP manual: fopen](https://www.php.net/manual/en/function.fopen.php)
- [PHP manual: fgets](https://www.php.net/manual/en/function.fgets.php)
- [PHP manual: explode](https://www.php.net/manual/en/function.explode.php)
- [PHP manual: file_exists](https://www.php.net/manual/en/function.file-exists.php)