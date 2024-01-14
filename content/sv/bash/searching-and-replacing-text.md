---
title:    "Bash: Sökning och ersättning av text"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin behövt ändra text i en fil eller ett dokument? I så fall kan du dra nytta av Bash-progammeringens möjlighet att söka och ersätta text. Detta gör det möjligt att snabbt och effektivt ändra flera förekomster av en viss text utan att behöva göra det manuellt.

## Hur man gör det

Så här fungerar det: För att söka och ersätta text i en fil med hjälp av Bash, använder man kommandot `sed`. Det står för "stream editor" och är utformat för att utföra operationer på filströmmar, vilket innebär att den kan tolka och ändra text i en fil utan att man behöver öppna och redigera den manuellt.

Eftersom sök- och ersättningstolet är enkelt att läsa och skriva, kan du använda `sed` för att göra avancerade ändringar i en fil med bara några få linjer kod.

Låt oss till exempel säga att du har en textfil med titlar på filmer, men du vill ändra en del av filmtiteln för att bättre beskriva innehållet. Du vill ändra "Star Wars: The Empire Strikes Back" till "Star Wars: Episode V - The Empire Strikes Back". Istället för att öppna filen och manuellt ändra varje förekomst av filmtiteln, kan du använda `sed` för att göra detta på bara ett par sekunder:

```Bash
sed -i 's/The Empire Strikes Back/Episode V - The Empire Strikes Back/g' filnamn.txt
```

Låt oss gå igenom vad som händer här:

- `-i` - Detta flagga indikerar för `sed` att ändringarna ska göras direkt på den befintliga filen.
- `s` - Detta står för "ersätt" och är kommandot som instruerar sed att söka efter en viss text och ersätta den med en annan.
- 'The Empire Strikes Back' - Detta är den befintliga texten som ska ersättas.
- 'Episode V - The Empire Strikes Back' - Detta är den nya texten som ska ersätta den befintliga.
- `g` - Detta flagga indikerar för `sed` att alla förekomster av den befintliga texten ska ersättas, inte bara den första som hittas.

Som en snabb editeringsanteckning måste du se till att det inte finns några mellanslag mellan flaggorna och texten som de är kopplade till, och att alla flaggor är omgivna av citattecken.

Efter att ha kört detta kommando kommer du att se att filen nu har 'Episode V - The Empire Strikes Back' som filmtitel för hela filmen.

## Djupdykning

Förutom kommandon som vi använde ovan, finns det många andra flaggor och användningsområden för `sed`-kommandot. Du kan också använda Regex (reguljära uttryck) för att göra sökningen mer precist. Detta kan vara till hjälp om du behöver göra komplexa ändringar i en fil.

En annan användbar funktion i `sed` är möjligheten att ändra endast en viss del av en fil. Istället för att ersätta allt innehåll av en viss fil kan du använda flaggan `c` för att ersätta allt innehåll mellan två specifika linjer i filen.

## Se även

- Kom igång med shell-scripting med Bash (https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- Den kompletta guiden för `sed`-kommandot (https://www.linode.com/docs/tools-reference/tools/sed-beginners-guide/)
- Reguljära uttryck för Bash-programmerare (https://linuxacademy.com/blog/linux/regex-tips-tricks-for-the-bash-guru/)