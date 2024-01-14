---
title:                "Python: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga eller "concatenate" strängar är en viktig del av Python-programmering. Det låter dig kombinera flera strängar till en större sträng för att skapa mer dynamiska och anpassade texter för dina program.

## Hur man gör

För att sammanfoga strängar i Python, använder man "+" operatorn för att lägga till den andra strängen till den första. Här är ett exempel:

```Python
str1 = "Hej"
str2 = "världen!"
str3 = str1 + str2
print(str3)
```
Output: Hej världen!

Det är viktigt att komma ihåg att de två strängarna du försöker sammanfoga måste vara av samma datatyp, annars kommer koden att ge ett felmeddelande.

Du kan också använda "join" funktionen för att sammanfoga flera strängar. Här är ett exempel där vi sammanfogar en lista av strängar till en större sträng med hjälp av kommatecken som skiljer dem åt:

```Python
str_list = ["Hej", "på", "dig", "världen!"]
str3 = ",".join(str_list)
print(str3)
```
Output: Hej,på,dig,världen!

Det finns också möjlighet att formatera strängar med hjälp av "format" funktionen. Detta låter dig sätta in värden i en sträng från variabler eller användardata. Här är ett exempel där vi sätter in en variabel i en sträng:

```Python
name = "Emil"
str3 = "Välkommen, {}".format(name)
print(str3)
```
Output: Välkommen, Emil

## Djupdykning

En intressant aspekt av att sammanfoga strängar i Python är att det faktiskt inte skapar en ny sträng, utan returnerar en ny strängobjekt som innehåller de två ursprungliga strängarna. Detta kan vara viktigt att förstå när det gäller prestanda och minnesanvändning i större program.

En annan viktig aspekt är att strängar är oföränderliga (immutable) i Python, vilket betyder att de inte kan ändras på plats. Detta innebär att om du försöker ändra en enstaka del av en sträng, kommer det att skapas en helt ny strängobjekt istället för att ändra den befintliga. Det här kan också påverka prestandan i ditt program om du arbetar med stora mängder data.

## Se också

- [Officiell Python-dokumentation - Strings (str)](https://docs.python.org/sv/3.8/library/stdtypes.html#text-sequence-type-str)
- [GeeksforGeeks - String Concatenation](https://www.geeksforgeeks.org/python-string-concatenation/)
- [Real Python - Formatting Strings in Python](https://realpython.com/python-f-strings/)