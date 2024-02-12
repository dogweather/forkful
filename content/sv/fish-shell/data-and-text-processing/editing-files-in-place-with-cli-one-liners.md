---
title:                "Redigera filer på plats med kommandoradsenradare"
aliases:
- /sv/fish-shell/editing-files-in-place-with-cli-one-liners/
date:                  2024-01-27T16:20:42.495855-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redigera filer på plats med kommandoradsenradare"

tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/editing-files-in-place-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att redigera filer direkt med CLI-engreppsrader handlar om att göra ändringar direkt i filer från kommandoraden, utan att öppna dem i en textredigerare. Programmerare gör detta för att spara tid och automatisera repetitiva redigeringsuppgifter, vilket gör deras arbetsflöde smidigare och mer effektivt.

## Hur man gör:

Fish Shell, känt för sina användarvänliga funktioner och kraftfulla skriptmöjligheter, erbjuder flera sätt att redigera filer direkt. Dock, till skillnad från vissa andra skal, har Fish inte en inbyggd mekanism för direkta redigeringar (`sed -i` i Bash, till exempel). Men frukta inte, du kan fortfarande uppnå detta med lite kreativitet och hjälp från externa verktyg som `sed` och `awk`.

### Använda `sed` för enkla ersättningar
För att ersätta alla instanser av "hello" med "world" i `file.txt`, skulle du använda:
```Fish Shell
sed -i '' 's/hello/world/g' file.txt
```

### Använda flera `sed`-kommandon
Om du behöver utföra flera ersättningar kan du kedja dem så här:
```Fish Shell
sed -i '' -e 's/fish/bass/g' -e 's/rainbow/trout/g' file.txt
```

### Använda `awk` för mer komplexa operationer
För operationer som är för komplexa för `sed`, kanske `awk` är verktyget du föredrar. Så här dubblar du numret på varje rad:
```Fish Shell
awk '{print $1 * 2}' file.txt > temp && mv temp file.txt
```

### Notering om felhantering
Kom ihåg, när du använder dessa verktyg från Fish, är det avgörande att fånga upp fel och förstå deras meddelanden. Använd Fishs robusta felhantering för att göra dina skript mer tillförlitliga.

## Fördjupning

Historiskt sett har direkt redigering av filer varit en grundpelare i Unix- och Linux-programmering, som erbjuder ett effektivt sätt att utföra snabba redigeringar utan att manuellt öppna filer. Verktyg som `sed` och `awk` är ärevördiga verktyg som har funnits sedan Unixs tidiga dagar, och har blivit oumbärliga för textbearbetningsuppgifter.

Fish Shell, som är mer modernt och skryter med förbättringar i användbarhet och skriptning, saknar inbyggd direkt redigering främst på grund av dess designfilosofi som är inriktad på interaktivitet och användarvänlighet. Avsaknaden av ett inbyggt direktredigeringskommando i Fish understryker vikten av externa verktyg i Unix-liknande ekosystem.

Alternativ för direkt redigering i Fish inkluderar användning av temporära filer eller att utnyttja Perl eller Python-engreppsrader, som kan erbjuda mer flexibilitet eller läsbarhet för komplexa uppgifter.

Till exempel, med hjälp av Perl:
```Fish Shell
perl -pi -e 's/find/replace/g' file.txt
```
Eller Python:
```Fish Shell
python -c "import re, sys; [sys.stdout.write(re.sub('pattern', 'replacement', line)) for line in sys.stdin]" < file.txt > temp && mv temp file.txt
```

När det gäller genomförande, när du utför direkt redigering, skapar dessa verktyg vanligtvis en temporär fil under ytan, skriver ändringarna där, och ersätter sedan den ursprungliga filen med den modifierade versionen. Detta tillvägagångssätt säkerställer att filredigeringsprocessen inte korrumperar eller förlorar data om ett fel uppstår under operationen.

Att förstå dessa verktyg och metoder låter programmerare som använder Fish Shell effektivt införliva direktredigering i sina skript, vilket överbryggar klyftan mellan Fishs användarvänliga funktioner och den råa kraften hos traditionella Unix textbearbetningsverktyg.
