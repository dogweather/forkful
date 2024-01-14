---
title:    "Ruby: Borttagning av tecken som matchar ett mönster"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort karaktärer som matchar ett mönster kan vara användbart för att rensa upp data eller filtrera ut oönskad information från en sträng eller lista. Detta är också en vanlig uppgift när man arbetar med textbaserade applikationer eller analyserar data.

## Så här gör du

För att ta bort karaktärer som matchar ett visst mönster i Ruby kan vi använda metoden `gsub` (global substitution). Metoden tar två argument: ett mönster att matcha och vad som ska ersätta mönstret.

Här är ett exempel där vi tar bort alla siffror från en sträng:

```Ruby
sträng = "Det är 2021, och Ruby är fortfarande populärt!"
puts sträng.gsub(/\d+/, '')
```

Detta kommer att ge utmatningen: "Det är , och Ruby är fortfarande populärt!" Eftersom mönstret `\d+` matchar alla siffror i strängen och vi ersätter dem med en tom sträng.

## Djupdykning

När vi använder `gsub`, kan vi också utnyttja en väldigt kraftfull del av Ruby, nämligen reguljära uttryck (regular expressions). Reguljära uttryck är mönster vi kan använda för att söka igenom strängar och matcha specifika karaktärer eller mönster. I vårt exempel ovan använde vi redan ett reguljärt uttryck, `\d+`, som betyder "en eller flera siffror".

Här är några fler exempel på hur vi kan ta bort karaktärer som matchar ett mönster med hjälp av reguljära uttryck:

- `/\s+/` - här tar vi bort alla mellanslag från en sträng.
- `/[a-z]+/` - detta kommer att ta bort alla bokstäver från en sträng.
- `/.*/` - denna regex kommer att ta bort allt innehåll från en sträng.

Reguljära uttryck kan verka överväldigande till en början, men de är väldigt användbara när man arbetar med textdata. Det finns många guider och kurser online för att lära sig mer om hur man skapar och använder reguljära uttryck i Ruby.

## Se även

- [Reguljära uttryck i Ruby](https://rubytutorial.io/ruby-regular-expressions/)
- [Dokumentation om `gsub` metoden](https://ruby-doc.org/core-3.0.2/String.html#method-i-gsub)
- [Online regex-testare för att experimentera med olika mönster](https://rubular.com/)

Nu har du lärt dig hur du kan ta bort karaktärer som matchar ett visst mönster i Ruby. Detta kan vara användbart i många olika situationer, så fortsätt utforska och öva på att använda `gsub` och reguljära uttryck för att manipulera strängar och textdata. Lycka till!