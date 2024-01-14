---
title:    "Arduino: Hitta längden av en sträng"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en viktig del av programmering, särskilt när man arbetar med textbaserad data. Det kan användas för att kontrollera att en sträng är inom en viss längd, dela upp en sträng i mindre delar eller jämföra två strängar.

## Hur man gör det

Att hitta längden på en sträng på Arduino är enkelt med hjälp av funktionen `strlen()`. Detta är en inbyggd funktion som kan användas för att beräkna längden på en sträng. Här är ett exempel på hur man kan använda `strlen()` för att hitta längden på en sträng som heter "Hej Arduino":

```Arduino
char str[] = "Hej Arduino";
int längd = strlen(str);
```

I detta exempel tilldelas variabeln `längd` längden på strängen "Hej Arduino". Detta värde kommer att vara 11, eftersom det är antalet tecken i strängen.

Det är viktigt att notera att `strlen()` fungerar endast för "c-stringar", vilket innebär att den endast fungererar med null-terminerade strängar. Null-terminerade strängar har en specialtecken ('\0') i slutet för att indikera slutet på strängen. Om du använder en vanlig sträng (ex. String-objekt) kommer `strlen()` inte att fungera. I så fall kan du använda `str.length()` för att hitta längden på en vanlig sträng.

## Djupdykning

För en mer avancerad förståelse av hur `strlen()` fungerar, måste du förstå hur data lagras i minnet på en Arduino. Minnet på en Arduino kan delas upp i "bytes", som är 8-bitars block av data. En char-variabel tar upp en byte i minnet och kan lagra ett tecken.

När en sträng lagras i minnet, lagras alla tecken i följd och ett sista null-tecken läggs till i slutet. När `strlen()` körs, räknar den antalet tecken tills den når det sista null-tecknet och returnerar sedan det värdet.

Det finns också andra metoder för att hitta längden på en sträng, som att räkna antalet loopar som krävs för att nå null-tecknet eller att använda en variabel för att hålla reda på längden medan man loopar igenom strängen. Men `strlen()` är den enklaste och mest effektiva metoden för att hitta längden på en sträng.

## Se även

Här är några användbara länkar för att hjälpa dig förstå mer om hur man hittar längden på en sträng:

- [strlen() referens](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [Fördjupningsguide för funktioner med strängar](https://www.arduino.cc/en/Tutorial/StringLengthOperator)
- [Skillnaden mellan null-terminerade strängar och vanliga strängar](https://hackaday.com/2018/01/24/running-functional-string-tests-with-both-standard-and-null-terminated-strings/)

Lycka till med att utforska hur man hittar längden på en sträng på Arduino!