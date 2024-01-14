---
title:    "PHP: Att använda reguljära uttryck"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
I programvaruvärlden använder vi ofta strängar (eller strings på engelska) för att lagra eller manipulera data på olika sätt. Regular expressions, eller regex, är en kraftfull teknik som tillåter oss att söka, matcha och ersätta specifika mönster i dessa strängar. Med hjälp av regex kan vi skapa mer exakta sökningar och automatisera uppgifter som annars skulle vara mycket repetitiva. 

## Hur man använder regex i PHP
För att använda regex i PHP behöver du bara använda funktionen `preg_match` och ange det mönster du vill söka efter som första argument och strängen du vill söka igenom som det andra argumentet. Sedan kan du välja att lagra resultatet i en variabel eller bara använda det direkt. 

Exempel: 
```PHP 
$pattern = "/hello/"; // söker efter ordet "hello" 
$string = "Hello World"; // söker igenom "Hello World" 
if (preg_match($pattern, $string)) { // använder preg_match funktionen 
    echo "Ordet hello hittades!"; // output: "Ordet hello hittades!"
}
```

Du kan också använda regex för att ersätta en del av en sträng med en annan. För detta kan du använda funktionen `preg_replace`.

Exempel: 
```PHP 
$pattern = "/world/"; // söker efter ordet "world" 
$string = "Hello World"; // söker igenom "Hello World" 
$new_string = preg_replace($pattern, "universe", $string); // output: "Hello Universe" 
```

## Ta en djupare titt på regex
Regex kan verka förvirrande i början, men det finns många online-resurser som kan hjälpa dig att förstå det bättre. En bra plats att börja är genom att läsa dokumentationen för PHP-funktionen `preg_match` och `preg_replace`. Det finns också flera online regex verktyg som kan hjälpa dig att testa dina mönster och se vilka strängar de matchar.

## Se även
- [PHP Manual: preg_match](https://www.php.net/manual/en/function.preg-match.php)
- [PHP Manual: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [Regex101.com](https://regex101.com/) (online regex tester)
- [Learn Regular Expressions](https://www.regular-expressions.info/) (online regex tutorial)