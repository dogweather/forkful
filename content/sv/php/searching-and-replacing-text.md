---
title:    "PHP: Söka och ersätta text"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför
När vi skriver kod, finns det många tillfällen då vi behöver göra ändringar i texten. Ibland är det enkla förändringar som att byta ut ett ord, men ibland kan det vara mer komplicerade uppgifter som att ersätta flera delar av en text. I dessa situationer är det användbart att ha en funktion som kan söka och ersätta text automatiskt.

## Hur man gör det
För att söka och ersätta text i PHP kan vi använda funktionen `str_replace()`. Det tar tre parametrar: texten som ska sökas efter, texten som ska ersättas och den ursprungliga texten. Till exempel:

```PHP
<?php
$text = "Idag är en fin dag.";
$ersätt = "fint";
$text = str_replace("fin", $ersätt, $text);
echo $text;
?>
```
Detta kommer att ge utmatningen `Idag är en fint dag.` Denna funktion kan också användas för att byta ut flera delar av en text genom att ge den en array av sök- och ersättningsvärden som parameter:

```PHP
<?php
$text = "Jag gillar äpplen och bananer.";
$sök = array("äpplen", "bananer");
$ersätt = array("apelsiner", "jordgubbar");
$text = str_replace($sök, $ersätt, $text);
echo $text;
?>
```

Detta kommer att ge utmatningen `Jag gillar apelsiner och jordgubbar.` Det är också möjligt att använda en variabel som sökvärdet istället för statiska strängar, vilket gör det enklare att dynamiskt byta ut text beroende på olika förutsättningar.

## Djupdykning
Förutom `str_replace()` finns det också andra PHP-funktioner som kan användas för att söka och ersätta text, såsom `preg_match()` och `preg_replace()` som gör det möjligt att använda reguljära uttryck för att matcha och ersätta text. Det finns också olika parametrar som kan läggas till för att göra sökandet och ersättningen mer exakt, såsom att ignorera skillnader mellan stora och små bokstäver eller bara söka efter hela ord.

## Se även
- [PHP str_replace() funktionen](https://www.php.net/manual/en/function.str-replace.php)
- [PHP preg_match() funktionen](https://www.php.net/manual/en/function.preg-match.php)
- [PHP preg_replace() funktionen](https://www.php.net/manual/en/function.preg-replace.php)