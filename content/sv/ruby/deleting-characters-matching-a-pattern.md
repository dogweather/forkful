---
title:    "Ruby: Radera tecken som matchar mönstret"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför

Ibland när du skriver kod kan du komma över en situation där du vill ta bort specifika tecken från en sträng eller array. Det kan vara för att uppnå ett visst format eller för att filtrera bort oönskade tecken. I sådana fall kan det vara till hjälp att ha kunskap om hur man tar bort tecken som matchar ett visst mönster.

## Hur man gör det

För att ta bort tecken som matchar ett visst mönster kan du använda funktionen `gsub` i Ruby. Låt oss säga att vi har en sträng "Ruby är så kul!!!", och vi vill ta bort alla utropstecknen från den. Vi skulle då kunna använda följande kod:

```Ruby
sträng = "Ruby är så kul!!!"
sträng.gsub("!", "")
```

Outputen av koden skulle vara "Ruby är så kul". Detta beror på att `gsub` ersätter alla förekomster av det första argumentet med det andra argumentet, vilket i detta fall är en tom sträng.

Om vi istället vill ta bort alla siffror från en sträng kan vi använda ett reguljärt uttryck i `gsub`-funktionen. Detta mönster indikerar att alla siffror ska ersättas med en tom sträng. Koden skulle se ut så här:

```Ruby
sträng = "R0bby är bäst"
sträng.gsub(/\d/, "")
```

Outputen skulle vara "Rbby är bäst", eftersom siffran "0" har tagits bort från strängen.

## Djupdykning

`gsub`-funktionen är väldigt kraftfull eftersom den också kan ta emot ett block som argument. Detta gör det möjligt för oss att göra mer avancerade operationer på strängen eller arrayen. Till exempel kan vi använda detta för att konvertera alla bokstäver i en sträng till versaler:

```Ruby
sträng = "ruby är så kul"
sträng.gsub(/[a-z]/) { |bokstav| bokstav.upcase }
```

Outputen från detta kodblock skulle vara "RUBY ÄR SÅ KUL".

För att lära dig mer om reguljära uttryck och `gsub`-funktionen rekommenderar jag att du tittar på dokumentationen för Ruby eller läser mer om ämnet på nätet.

## Se även

- [Ruby Documentation](https://ruby-doc.org/)
- [Reguljära uttryck tutorial](https://www.regular-expressions.info/tutorial.html)