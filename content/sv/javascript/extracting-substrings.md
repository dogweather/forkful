---
title:                "Extrahering av delsträngar"
html_title:           "Javascript: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Varför skulle någon vilja extrahera substrängar i JavaScript? Det finns många användbara skript där substrängar kan användas, till exempel för att manipulera eller analysera textdata.

## Hur det fungerar
Det finns flera sätt att extrahera substrängar i JavaScript, beroende på vad du vill uppnå. Här är några exempel med kod och tillhörande utdata:

### Exempel 1: Extrahera en delmängd av en sträng
```javascript
let str = "Hej världen!";
let subStr = str.substring(4);
console.log(subStr); // Output: världen!
```

I det här exemplet tar vi en sträng och använder metoden `substring()` för att extrahera allt efter det fjärde tecknet. Detta resulterar i att vi bara får ut "världen!" delen av strängen.


### Exempel 2: Extrahera en delmängd baserat på positioner
```javascript
let str = "JavaScript är roligt!";
let subStr = str.substring(0, 10);
console.log(subStr); // Output: JavaScript
```

Här använder vi fortfarande `substring()` metoden, men denna gång specificerar vi både start och slutpositionen för den del vi vill extrahera. I detta fall får vi ut de första 10 tecknen, vilket ger oss "JavaScript" som utdata.

## Djupdykning
Som du kanske har märkt har vi använt metoden `substring()` för att utföra extraheringen i båda exemplen. Men det finns också andra metoder som kan användas för samma syfte, såsom `slice()` och `substr()`. Skillnaden ligger i vilka argument som används för att specifiera den del som ska extraheras.

- `substring()` tar emot två argument: startposition och slutposition. Om inget slutposition ges kommer det att använda slutet av strängen som standard.
- `slice()` tar emot två argument: startposition och slutposition. Om någon av positionerna är negativ kommer den att räknas bakifrån istället för framifrån.
- `substr()` tar emot två argument: startposition och längden på den del som ska extraheras.

En annan viktig skillnad är att `substr()` är den enda metoden som kan hantera negativa startpositioner, vilket kan vara användbart i vissa fall.

## Se även
- [JavaScript Strings Reference](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/String) - en djupare titt på stränghantering med JavaScript.
- [W3Schools: JavaScript substrings](https://www.w3schools.com/jsref/jsref_substring.asp) - fler exempel och övningar för att extrahera substrängar i JavaScript.