---
title:                "Javascript: Sökning och ersättning av text"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift inom Javascript-programmering. Genom att använda en sök-och-ersätt-funktion kan du enkelt ändra texten på många platser i din kod, vilket sparar tid och minskar risken för felaktig stavning.

## Så här gör du

För att söka och ersätta text i Javascript använder du metoden `replace()`. Dess syntax är enkel: du anger först den text du vill söka efter och sedan den text du vill ersätta den med.


```Javascript
let str = "Välkommen till min blogg!";
let newStr = str.replace("blogg", "hemsida");
console.log(newStr);
```

Output:
```Javascript
Välkommen till min hemsida!
```

Funktionen `replace()` tar även emot en så kallad *regular expression* (regex) som sökparameter. Med hjälp av regex kan du utföra avancerade sökningar och ersättningar baserat på mönster i texten.

```Javascript
let str = "Dagens datum är 2021-10-15.";
let newStr = str.replace(/\d{4}-\d{2}-\d{2}/, "YYYY-MM-DD");
console.log(newStr);
```

Output:
```Javascript
Dagens datum är YYYY-MM-DD.
```

Du kan också använda en global flagga `/g` för att söka och ersätta alla förekomster av en viss text.

```Javascript
let str = "Jag älskar äpplen och äppelpaj!";
let newStr = str.replace(/äpple/g, "banan")
console.log(newStr);
```

Output:
```Javascript
Jag älskar bananer och bananpaj!
```

## Djupdykning

När du använder `replace()`-metoden är det viktigt att förstå hur den fungerar. Standardmässigt ersätts bara den första förekomsten av den sökta texten. Om du vill ersätta alla förekomster måste du använda en global flagga, som vi visade i exemplet ovan.

En annan viktig aspekt att tänka på är att `replace()`-metoden returnerar en ny sträng och inte ändrar den befintliga strängen. Om du vill spara ändringarna måste du tilldela den nya strängen till en variabel.

Det finns också andra metoder inom Javascript som kan användas för att söka och ersätta text, som `replaceAll()` och `substr()`. Det är alltid en bra idé att utforska olika möjligheter och hitta den som passar bäst för ditt specifika fall.

## Se även

Här är några länkar som kan vara till hjälp när du ska söka och ersätta text i Javascript:

- [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools](https://www.w3schools.com/js/js_string_replace.asp)
- [Regex Tutorial](https://www.regular-expressions.info/javascript.html)