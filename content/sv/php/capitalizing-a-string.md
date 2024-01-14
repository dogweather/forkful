---
title:                "PHP: Kapitalisera en sträng"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Varför
Ibland behöver vi konvertera en sträng så att den börjar med en stor bokstav. Detta kan vara för att göra den mer läsbar eller för att följa specifika konventioner inom programmering.

# Hur man gör
Att konvertera en sträng till stora bokstäver i PHP är enkelt med hjälp av funktionen `ucfirst()`. Denna funktion tar emot en sträng som parameter och returnerar en ny sträng där den första bokstaven är stor. Här är ett exempel:

```PHP
$str = "hallå världen";
echo ucfirst($str); // Output: Hallå världen
```

För att konvertera varje ord i en sträng till stora bokstäver kan vi använda funktionen `ucwords()`. Den tar också en sträng som parameter och returnerar en ny sträng där varje ord börjar med en stor bokstav. Här är ett exempel:

```PHP
$str = "hej på dig";
echo ucwords($str); // Output: Hej På Dig
```

# Djupdykning
Det finns även andra funktioner och metoder inom PHP som kan användas för att konvertera en sträng till stora bokstäver. Till exempel `strtoupper()` som returnerar en sträng där alla bokstäver är stora eller `mb_convert_case()` som kan hantera flerspråkiga strängar.

Det är också viktigt att notera att olika språk och plattformar kan ha olika standarder för att konvertera strängar till stora bokstäver. Det är därför viktigt att se över dokumentationen för det specifika språket eller plattformen du arbetar med.

# Se även
- [PHP's officiella dokumentation för ucfirst()](https://www.php.net/manual/en/function.ucfirst.php)
- [PHP's officiella dokumentation för ucwords()](https://www.php.net/manual/en/function.ucwords.php)
- [PHP's officiella dokumentation för strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [MB_STRING-dokumentationen för mb_convert_case()](https://www.php.net/manual/en/function.mb-convert-case.php)