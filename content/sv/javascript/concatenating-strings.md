---
title:                "Javascript: Sammanfogar strängar"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Om du är ny inom programmering, kan begrepp som "concatenating strings" verka förvirrande och obekväma. Men oroa dig inte för mycket, för det är faktiskt en mycket användbar teknik inom Javascript. Att konkatenera strängar låter dig kombinera flera textsträngar för att skapa en enda sträng som kan användas i din kod. Det är särskilt användbart när du behöver skapa dynamiska meddelanden eller textsträngar som innehåller variabler.

## Hur man gör

För att konkatenera strängar i Javascript använder du operatorn "+" mellan strängarna du vill slå samman. Låt oss säga att du vill skriva ut en hälsning till användaren med deras namn. Här är ett exempel på hur du kan göra det:

```javascript
// Definiera variabeln "name"
var name = "Alice";

// Skapa en hälsning med namnet
var greeting = "Hej " + name + "!" ;

// Skriv ut resultatet
console.log(greeting);
```

I detta exempel använder vi "+" för att kombinera tre strängar (Hej, Alice och !) till en enda sträng (Hej Alice!). I konsolen kommer du att se resultatet: "Hej Alice!".

Du kan också konkatenera strängar med hjälp av variabler som innehåller nummer eller booleska värden. I så fall konverteras dessa värden till strängar innan de kombineras med resten av strängarna. Här är ett annat exempel:

```javascript
var num = 10;
var message = "Jag har " + num + " äpplen.";

console.log(message);
```

I detta exempel kommer variabeln "num" att omvandlas till strängen "10" innan den kombineras med resten av strängen. Resultatet i konsolen blir: "Jag har 10 äpplen."

## Djupdykning

I Javascript kan du också använda metoden "concat()" för att kombinera flera strängar. Istället för att använda "+" operatorn använder du metoden på en av strängarna och lägger till de andra strängarna som argument. Här är ett exempel:

```javascript
var str1 = "Hello ";
var str2 = "world";

// Använda concat() metoden på str1
var str3 = str1.concat(str2, "!");

console.log(str3);
```

I detta exempel använder vi concat() metoden på str1 och lägger till str2 och "!" som argument. Resultatet blir samma som om vi hade använt "+" operatorn.

Det finns också andra metoder som kan användas för att manipulera strängar, såsom "slice()" och "substring()" som kan användas för att välja en del av en sträng att konkatenera. Det är viktigt att notera att strängar är oföränderliga i Javascript, vilket betyder att de inte kan ändras. När du manipulerar en sträng, kommer en ny sträng att skapas istället. Detta är anledningen till att det är viktigt att förstå hur konkatenering fungerar och att använda lämpliga metoder för att behålla önskat resultat.

## Se även

- [W3Schools - Concatenating strings in Javascript](https://www.w3schools.com/js/js_string_concat.asp)
- [MDN Web Docs - Working with strings in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Stack Overflow - What is the best way to concatenate strings in Javascript?](https://stackoverflow.com/questions/5612787/what-is-the-best-way-to-concatenate-strings-in-javascript)