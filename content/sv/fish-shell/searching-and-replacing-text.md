---
title:    "Fish Shell: Söka och ersätta text"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift för många programmerare. Det kan vara till nytta när man behöver ändra flera förekomster av ett visst ord eller när man vill rensa bort viss information från en fil. Med hjälp av Fish Shell kan man göra denna process snabbare och enklare.

## Hur man gör

Fisk Shell har ett inbyggt kommando som heter `sed` som står för "stream editor". Detta kommando kan användas för att söka och ersätta text i en fil eller en ström av data. Det går till på följande sätt:

```
Fish Shell 1.0.0
$ echo "Hello World!" > test.txt
$ sed 's/World/Fish/g' test.txt
Hello Fish!
```

I detta exempel använder vi `sed` för att ersätta ordet "World" med "Fish" i vår fil `test.txt`. Vi gör detta genom att använda kommandot `s/World/Fish/g`. Detta betyder att vi vill söka efter "World" och ersätta med "Fish" globalt, det vill säga i hela filen.

Man kan även använda regex eller reguljära uttryck för mer avancerade sökningar. Till exempel om man vill söka och ersätta alla siffror i en fil kan man göra följande:

```
Fish Shell 1.0.0
$ echo "I have 100 apples!" > test.txt
$ sed 's/[0-9]/X/g' test.txt
I have XXX apples!
```

I detta fall använder vi uttrycket `[0-9]` för att söka efter alla siffror och ersätta dem med bokstaven X.

## Djupdykning

När man använder `sed` i Fish Shell finns det några olika flaggor man kan använda för att anpassa sök- och ersättningsprocessen. Några vanliga flaggor är `-i` som gör ändringarna direkt i den ursprungliga filen, `-e` som låter dig använda flera sök- och ersättningsuttryck och `-r` som möjliggör användningen av regex utan att behöva använda `[]` runt uttrycken.

Det går också att kombinera `sed` med andra kommandon i Fish Shell för att göra mer avancerade sök- och ersättningsoperationer. Man kan till exempel använda `grep` för att söka efter en viss text i en fil och sedan använda `sed` för att ersätta den hittade texten.

## Se även

- [Fish Shell dokumentation](https://fishshell.com/docs/current/)
- [sed manual](https://www.gnu.org/software/sed/manual/sed.html)
- [RegEx-tutorial för nybörjare](https://regexone.com/)