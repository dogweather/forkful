---
title:    "Javascript: Att använda regelbundna uttryck"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför

Regular expressions är ett kraftfullt verktyg inom Javascript-programmering. Genom att använda sig av reguljära uttryck kan man effektivt söka, ersätta och manipulera textmönster i sina program. Detta kan spara mycket tid och göra koden mer lättläst.

## Hur man använder reguljära uttryck i Javascript

För att använda reguljära uttryck i Javascript behöver man först skapa ett reguljärt uttryck objekt med hjälp av följande syntax:

```Javascript
let re = /mönster/gi;
```

Detta skapar ett reguljärt uttryck som söker efter ett visst mönster, i det här fallet "mönster", i en sträng. Den "g" som kommer efter "/" indikerar att uttrycket ska vara globalt, det vill säga leta efter alla förekomster av mönstret och inte bara den första. Den "i" som kommer efter "g" gör att uttrycket ska vara skiftlägesokänsligt, det vill säga att det inte spelar roll om bokstäverna är stora eller små.

För att söka efter mönstret i en sträng använder man sig av metoden "test()":

```Javascript
let text = "Detta är en text med mönstret som ska matchas.";

console.log(re.test(text));
```

I detta fall kommer uttrycket att returnera "true" eftersom mönstret finns i strängen "text".

Man kan även använda reguljära uttryck för att söka efter meddelanden direkt i en sträng med hjälp av metoden "match()":

```Javascript
let text = "Här är en text med flera förekomster av mönstret.";

console.log(text.match(re));
```

Denna kod kommer att returnera en array med alla förekomster av mönstret i texten.

## En djupdykning i reguljära uttryck

Reguljära uttryck är inte bara användbara för att söka efter mönster, utan kan även användas för att manipulera och ersätta text i en sträng. Genom att använda sig av speciella symboler och uttryck kan man enkelt skapa mer avancerade reguljära uttryck.

Till exempel kan man använda sig av "()" för att gruppera delar av uttrycken och "$" för att referera till dessa grupper senare:

```Javascript
let re = /([A-Za-z]+)\s([A-Za-z]+)/;

let name = "John Doe";

let newName = name.replace(re, "$2, $1");

console.log(newName);
```

Denna kod kommer att ta bort mellanslaget i "John Doe" och byta plats på för- och efternamn, vilket resulterar i "Doe, John".

Det finns många fler användbara symboler och uttryck som kan hjälpa till att skapa mer avancerade och effektiva reguljära uttryck. En bra resurs för att lära sig mer är "RegExr" som kan hittas i "See Also"-avsnittet nedan.

## Se även

- [En interaktiv guide till reguljära uttryck](https://regexr.com/)
- [W3Schools guide till reguljära uttryck](https://www.w3schools.com/js/js_regexp.asp)
- [MDN web docs om reguljära uttryck i Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)