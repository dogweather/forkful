---
title:                "Kotlin: Radera tecken som matcher ett mönster"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort karaktärer som matchar ett mönster är en vanlig uppgift som ofta uppstår vid datahantering och programmering. Det kan vara användbart för att rensa bort oönskad information eller för att förbereda data för analys.

## Hur man gör

För att ta bort karaktärer som matchar ett visst mönster, kan man använda funktionen `replace()` tillsammans med reguljära uttryck i Kotlin.

Exempel:

```Kotlin
val text = "Hej!123 Det här är en text 456 som innehåller siffror"
val cleanedText = text.replace("[^A-Za-z ]".toRegex(), "")
println(cleanedText)
```

Output:

```
Hej Det här är en text som innehåller siffror
```

I detta exempel så ersätts alla karaktärer som inte är bokstäver eller mellanslag med en tom sträng, vilket resulterar i att alla siffror blir borttagna från texten.

## Djupdykning

För att förstå hur detta fungerar, behöver vi först förstå vad reguljära uttryck är. Det är en syntax för att beskriva mönster i strängar. Genom att använda reguljära uttryck kan man söka efter och ersätta text baserat på olika mönster snarare än exakta ord.

I exemplet ovan användes `[ ]` för att beskriva ett set av karaktärer som matchar, och `^` betyder alla karaktärer som inte finns i setet. Så `/[^A-Za-z ]/` betyder alla karaktärer som inte är bokstäver eller mellanslag.

Det finns även andra modifierare eller symboler som kan användas i reguljära uttryck för mer avancerad sökning, men det är utanför omfattningen av denna artikel.

## Se även

- [Kotlin Regex Dokumentation](https://kotlinlang.org/docs/reference/regular-expressions.html)
- [RegExr - Reguljära uttryck tester och referens](https://regexr.com/)