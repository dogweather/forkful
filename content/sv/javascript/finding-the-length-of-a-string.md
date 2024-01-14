---
title:                "Javascript: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att hitta längden på en sträng är en vanlig uppgift när man arbetar med Javascript-programmering. Det kan vara användbart när man behöver kontrollera om en inmatad text är tillräckligt lång eller för att trimma bort onödig text.

## Så här gör du
För att hitta längden på en sträng i Javascript, använder man metoden `.length`. Till exempel:

```Javascript
let string = "Hej, jag heter Anna!";
console.log(string.length);
```

Detta kommer att ge outputen `20`, eftersom det finns 20 tecken i den givna strängen, inklusive mellanslag.

Innan man kan använda `.length` metoden, måste man se till att strängen är lagrad i en variabel. Det är också viktigt att komma ihåg att `.length` metoden returnerar antalet tecken i en sträng, inte antalet ord.

```Javascript
let str1 = "Det här är en sträng med flera ord";
let str2 = "Enkelt";
console.log(str1.length); // output: 33
console.log(str2.length); // output: 6
```

Det är också möjligt att använda `.length` metoden på andra Javascript-datatyper, såsom arrayer.

## Djupdykning
Det finns flera sätt att hitta längden på en sträng i Javascript, men den vanligaste metoden är genom att använda `.length` metoden som vi nämnde tidigare.

En annan metod är att använda en `for-loop` och räkna antalet iterationer. Detta kan vara användbart för strängar som innehåller specifika tecken eller teckenkombinationer som man vill söka efter.

```Javascript
let string = "Hej, jag heter Anna!";
let count = 0;
for (let i = 0; i < string.length; i++) {
  if (string[i] === "e") {
    count++;
  }
}
console.log(count); // output: 2
```

Slutligen, är det viktigt att komma ihåg att `.length` metoden inte tar hänsyn till teckenkodning. Det betyder att flerbyte-tecken inte räknas som ett enda tecken, vilket kan ge en felaktig längd på strängen om man inte tar hänsyn till detta.

## Se även
- [Javascript: Strings](https://www.w3schools.com/js/js_strings.asp)
- [MDN Web Docs: String length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Tutorialeando: Como obtener la longitud de una cadena en JavaScript](https://www.tutorialeando.com/java-script/obtener-la-longitud-de-una-cadena-en-javascript/)