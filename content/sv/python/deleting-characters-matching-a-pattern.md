---
title:                "Python: Raderar tecken som matchar ett mönster"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Varför

Ibland när man skriver kod kan det hända att man behöver ta bort vissa tecken som matchar ett visst mönster. Det kan till exempel vara borttagning av interpunktion eller skiljetecken från en textsträng. I denna bloggpost kommer vi att titta på hur man enkelt kan ta bort sådana tecken med hjälp av Python-programmering.

##Hur man gör

För att ta bort tecken som matchar ett visst mönster i en textsträng använder vi oss av en Python-funktion som heter "replace()". Denna funktion tar två argument, det första är tecknet eller mönstret som vi vill ta bort och det andra är vad vi vill ersätta det med. Låt oss titta på ett exempel:

```Python
text = "Hej! Hur går det?"
print(text.replace("!", "")) 
```

I detta exempel ersätter vi utropstecknet med ett tomt sträng. Resultatet blir då "Hej Hur går det?" eftersom utropstecknet har tagits bort från texten.

Om vi vill ta bort flera tecken kan vi även använda oss av "regex" (regular expressions) när vi använder "replace()". Med regex kan vi definiera ett mönster för vilka tecken som vi vill ta bort från texten. Låt oss titta på ett exempel där vi vill ta bort alla siffror från en textsträng:

```Python
import re
text = "Jag är 25 år gammal."
print(re.sub('[0-9]', '', text))
```

I detta exempel har vi använt oss av funktionen "sub()" från modulen "re" för att matcha och ta bort alla siffror från texten. Resultatet blir "Jag är år gammal." eftersom siffrorna har tagits bort från texten.

##Djupdykning

Som vi nämnde tidigare kan vi använda oss av regex när vi vill ta bort tecken som matchar ett visst mönster från en textsträng. Regex är en avancerad teknik som erbjuder många olika möjligheter för att söka och manipulera textsträngar. Det finns en hel del olika symboler och funktioner som man kan använda sig av, men här är några grundläggande saker att komma ihåg:

- "[ ]" används för att definiera ett mönster av tecken som vi vill matcha
- "^" används för att ange att mönstret endast ska matcha från början av strängen
- "$" används för att ange att mönstret endast ska matcha från slutet av strängen
- "." används för att matcha ett vilket som helst tecken
- "*" används för att matcha 0 eller flera av föregående tecken
- "+" används för att matcha 1 eller flera av föregående tecken

Det finns många fler symboler och funktioner som man kan använda sig av i regex, men detta ger en bra grund att börja med. Om du vill lära dig mer om regex och dess möjligheter kan du kolla in dessa resurser:

- [En grundläggande guide till regex](https://blog.finxter.com/regex/)
- [RegExr - ett verktyg för att testa dina regex-uttryck](https://regexr.com/)
- [Tutorialspoint - en komplett guide till regex](https://www.tutorialspoint.com/python/python_reg_expressions.htm)

##Se även

- [Python-dokumentationen om "replace()"](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Python-dokumentationen om modulen "re"](https://docs.python.org/3/library/re.html)
- [En tutorial om regex med Python](https://realpython.com/regex-python/)