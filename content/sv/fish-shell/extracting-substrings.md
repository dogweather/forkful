---
title:                "Fish Shell: Extrahera substränger"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

Vad är Fish Shell substring-extrahering och varför bör du använda det?

I denna bloggpost kommer vi att utforska hur man kan extrahera substränger med hjälp av Fish Shell och varför detta kan vara en användbar funktion för programmerare. Vi kommer att gå igenom hur man utför detta steg-för-steg med hjälp av kodexempel och djupare information om substrängar.

## Varför

Att extrahera substrängar kan vara användbart när du behöver bearbeta textsträngar på ett specifikt sätt. Det kan vara att extrahera delar av en telefonnummersträng, eller kanske ett datum från en längre textsträng. Genom att använda Fish Shell för att extrahera substrängar kan du enkelt göra detta utan att behöva öppna en annan textredigerare eller använda komplexa regex uttryck.

## Hur To

För att extrahera substrängar i Fish Shell kan du använda inbyggda kommandon som "string sub" eller "string match". Låt oss ta en titt på ett exempel där vi vill extrahera ett datum från en längre textsträng:

```
Fish Shell:

set text "Dagens datum är 2021-10-25"
set date (echo $text | string sub -b 3 10)
echo $date

Output: 2021-10-25
```

I detta exempel använder vi kommandot "string sub" för att extrahera substrängen från position 3 till 10 i vår textsträng. Vi använder också flaggan -b för att specificera att vi vill extrahera substrängen baserat på antal bokstäver istället för antal tecken. Detta resulterar i att vi får ut datumet som vi ville.

En annan användbar funktion för att extrahera substrängar är "string match". Låt oss säga att vi har en lista med filnamn som alla följer samma mönster, och vi vill extrahera bara filnamnen utan filändelserna:

```
Fish Shell:

set filenames "doc1.txt doc2.pdf doc3.docx"
for filename in $filenames
    echo (echo $filename | string match '[^.]*')

Output: doc1 doc2 doc3
```

## Djupdykning

För att extrahera substrängar med hjälp av "string sub" eller "string match" kommandon, behöver du ha kunskap om deras syntax och flaggor. Du kan läsa mer om detta i Fish Shell dokumentation eller genom att använda hjälpkommandot inuti själva terminalen (type "help string sub" eller "help string match"). Det är också viktigt att förstå skillnaden mellan att extrahera baserat på tecken och bokstäver, vilket kan påverka resultatet beroende på din textsträng.

## Se också

För mer information om Fish Shell och dess användbara funktioner, kolla in följande länkar:

- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell guide för nya användare](https://fishshell.com/docs/current/tutorial.html)
- [Officiella Fish Shell Github repo](https://github.com/fish-shell/fish-shell)
- [Fish Shell community och support](https://fishshell.com/docs/current/support.html)