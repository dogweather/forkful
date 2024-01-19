---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# Sammansättning av strängar med JavaScript
### Översättning till svenska och innehåll som är relevant för svenska läsare.

## Vad & Varför?
Sammansättning av strängar är det sätt som vi lägger samman två eller flera strängar till en. Varför gör vi det? För att snabbt och enkelt skapa nya strängar från redan befintliga bitar av information.

## Så här gör du:
Nu över till koden. Här är några exempel på hur du kan använda JavaScript för att sätta samman strängar:

```Javascript
    var hej = "God morgon";
    var namn = " Bob";
    var hälsning = hej + namn;
    console.log(hälsning);
```

Resultatet blir "God morgon Bob".

Ett annat sätt är att använda `concat()` metoden:

```Javascript
    var hej = "God morgon";
    var namn = "Bob";
    var hälsning = hej.concat(namn);
    console.log(hälsning);
```

Resultatet blir också "God morgonBob".

Och det senaste sättet, med hjälp av `Template literals`:

```Javascript 
    var hej = "God morgon";
    var namn = "Bob";
    var hälsning = `${hej} ${namn}`;
    console.log(hälsning);
```

Resultatet blir "God morgon Bob".

## Djupdykning
Förenklat sagt, sammansättning av strängar har varit en del av JavaScript sedan det första lanserades som LiveScript 1995. Men metoder för att åstadkomma det har utvecklats över åren. Till exempel, `concat()` metoden, `+` operatorn och `Template literals`.

När det gäller alternativ finns det andra programmeringsspråk som också erbjuder funktioner för strängsammansättning, till exempel Python med sin format-metod och C# med sin `StringBuilder` klass.

När det kommer till implementationen, när vi sätter samman strängar med `+` operatorn eller `concat()` metoden, skapar JavaScript egentligen nya strängar eftersom strängar i JavaScript är oföränderliga. Men när vi använder `Template literals`, bygger JavaScript egentligen en enda sträng utifrån de givna literals och uttrycken.

## Se Även
- [MDN Web Docs: String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#string_operators)
- [JavaScript.info: String concatenation](https://javascript.info/string#string-concatenation-binary-plus)
- [Stack Overflow: JavaScript string concatenation and performance](https://stackoverflow.com/questions/7299010/why-is-string-concatenation-faster-than-array-join)