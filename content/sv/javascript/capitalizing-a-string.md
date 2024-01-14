---
title:    "Javascript: Att stora bokstäver i en sträng"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
Du kanske har stött på situationer där du behöver ändra en sträng så att bara den första bokstaven är stor bokstav och resten är små bokstäver. Detta kallas att "kaptalisera" en sträng och det kan ha olika syften, till exempel när du vill förbättra läsbarheten eller för att matcha en specifik standard i din kod.

## Hur man gör
Det finns flera sätt att kaptalisera en sträng i Javascript, men här är två enkla exempel som du kan utforska:

```Javascript
// Med hjälp av "toUpperCase()" och "toLowerCase()" metoder
let str = "hej på dig!";
let capitalizedString = str[0].toUpperCase() + str.substring(1).toLowerCase(); // Output: "Hej på dig!"

// Med hjälp av "charAt()" och "slice()" metoder
function capitalizeString(string) {
  return string.charAt(0).toUpperCase() + string.slice(1).toLowerCase();
}

console.log(capitalizeString("hej på dig!")); // Output: "Hej på dig!"
```

Det första exemplet använder inbyggda metoder i Javascript för att göra den första bokstaven stor och resten små. Det andra exemplet skapar en funktion som tar en sträng som argument och använder olika metoder för att returnera en kaptaliserad version av strängen. Du kan välja det alternativ som passar dig bäst beroende på dina behov och preferenser.

## Djupdykning
Om du vill lära dig mer om hur kaptalisering av strängar fungerar i Javascript, kan vi ta en titt på det andra exemplet som visar användningen av "charAt()" och "slice()" metoder.

Först tar "charAt()" metoden ett index som argument och returnerar tecknet på det specifika indexet. I vårt exempel är indexet alltid 0 eftersom vi bara vill kaptalisera den första bokstaven.

Därefter används "slice()" metoden för att skapa en ny sträng från en viss startpunkt till ett visst slutpunkt. Eftersom vi vill ha resten av strängen efter den första bokstaven, är vår startpunkt 1 och slutpunkten är inte specificerad, vilket betyder att vi använder standardvärdet som är slutet på strängen.

Slutligen kombineras den kaptaliserade första bokstaven med resten av strängen genom att enkelt lägga ihop dem med "+" operatören.

## Se även
Här är några länkar som kan vara användbara vid kapitalisering av strängar i Javascript:

- [String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [String.prototype.charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)