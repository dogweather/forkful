---
title:                "Fish Shell: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Att ladda ner en webbsida kan vara användbart för att spara innehållet offline eller för att utföra olika typer av datautvinning. Det kan också vara ett sätt att öva på Fish Shell-programmering och använda olika kommandon.

## Hur man gör det

För att ladda ner en webbsida i Fish Shell, använder man kommandot `curl`. Detta kommando kan ta emot en URL som input och sedan hämta den angivna sidan. Till exempel:

```Fish Shell
curl https://www.example.com
```

Detta kommer att ladda ner innehållet på hemsidan www.example.com och skriva ut det i terminalen.

Om man vill spara resultatet som en fil istället för att skriva ut det i terminalen, kan man använda flaggan `-o`. Till exempel:

```Fish Shell
curl https://www.example.com -o example.html
```

Detta kommer att spara innehållet på www.example.com i en fil som heter example.html. Man kan också använda andra flaggor som `-L` för att följa omdirigeringar eller `-s` för att tysta utmatningen.

## Djupdykning

När man laddar ner en webbsida med `curl` i Fish Shell, hämtas sidans HTML-kod tillsammans med eventuella CSS och JavaScript-filer som ligger på sidan. Detta betyder att man kan utföra olika typer av datautvinning och manipulation på den nedladdade filen.

En annan användbar funktion är att man kan använda regex (regular expressions) tillsammans med `curl` för att filtrera bort eller hämta specifika delar av innehållet på sidan. Man kan även använda kommandon som `cut` eller `grep` för att bearbeta den nedladdade filen och få ut önskat resultat.

## Se även

- Fish Shell's officiella dokumentation för `curl`: https://fishshell.com/docs/current/cmds/curl.html
- En guide för grundläggande datautvinning med `curl`: https://dev.to/febuddle/extracting-data-from-a-webpage-with-curl-3mf5