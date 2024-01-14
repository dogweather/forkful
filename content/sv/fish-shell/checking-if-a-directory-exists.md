---
title:    "Fish Shell: Kontrollera om en katalog finns."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför 

Att kontrollera om en mapp existerar är ett viktigt steg i programmering, särskilt vid skriptning av uppgifter som kräver åtkomst till specifika mappar. Det är också ett användbart sätt att undvika felaktig inmatning eller felaktig hantering av filstrukturer.

## Så här gör du 

```Fish Shell 
if test -d /sökväg/till/mapp
echo "Mappen existerar"
else 
echo "Mappen finns inte"
```

Detta enkla kodexempel visar hur man kan använda testkommandot i Fish Shell för att kontrollera om en mapp existerar. Om mappen finns kommer meddelandet "Mappen existerar" att skrivas ut, annars kommer "Mappen finns inte" att visas. Detta kan naturligtvis anpassas för att passa till olika behov, till exempel kan du ange en annan åtgärd att utföra om mappen inte existerar.

## Utforska Mer 

Kontroll av mappars existens kan också uppnås med andra metoder såsom att kontrollera returvärdet från find-kommandot eller använda en loop för att gå igenom listan över tillgängliga mappar. Det kan också vara användbart att lära sig om flaggor som -d, -e och -f som kan användas med testkommandot för att kontrollera mappars och filers olika egenskaper. 

## Se även

- [Fish Shell dokumentation] (https://fishshell.com/docs/current/commands.html?highlight=test)
- [Linux-testkommando] (https://www.computerhope.com/unix/utest.htm)
- [GeeksForGeeks-tutorial om kontroll av mappars existens i Fish Shell] (https://www.geeksforgeeks.org/check-if-a-directory-exists-in-fish-shell/)