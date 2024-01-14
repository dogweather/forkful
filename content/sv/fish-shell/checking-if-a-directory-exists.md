---
title:                "Fish Shell: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Ibland kan det vara nödvändigt att kontrollera om en viss mapp eller katalog existerar innan man fortsätter med en viss uppgift i Fish Shell-programmering. Att ha ett sätt att enkelt avgöra detta kan göra koden mer effektiv och tillförlitlig.

## Hur man gör det

Att kontrollera om en mapp existerar i Fish Shell är enkelt. Här är ett exempel där vi kontrollerar om mappen "dokument" finns i vår nuvarande arbetsmapp:

```Fish Shell
if test -d dokumentl
echo "Mappen dokument finns"
end
```

Om mappen faktiskt finns, kommer den första raden i koden att returnera "true" och den andra raden kommer att skrivas ut. Om mappen inte finns, kommer programmet helt enkelt att fortsätta utan att skriva ut något.

## Djupdykning

För att förstå hur detta fungerar måste vi först förstå vad "test -d" betyder. "Test" är ett inbyggt kommando i Fish Shell som används för att utföra olika test på filer eller mappar. "-d" står för "directory" och används för att testa om ett visst objekt är en mapp. Om det är en fil, skriver du istället "-f".

I vårt exempel använder vi också "if" och "end" vilket är Fish Shells sätt att skapa en "om-sats". Härav läser programmet som följer:

Om mappen "dokument" existerar i den nuvarande arbetsmappen, returnera "true" och skriv ut "Mappen dokument finns". Annars, fortsätt utan att skriva ut något ytterligare.

Detta kan också lätt anpassas för att kontrollera mappar i andra platser, till exempel:

```Fish Shell
if test -d /hem/användare/dokument
echo "Mappen dokument finns"
end
```

Här kommer Fish Shell att leta efter mappen "dokument" i den angivna sökvägen. Om den finns, kommer den att skriva ut "true" och "Mappen dokument finns". Annars, fortsätter den utan att göra något.

## Se även

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Effektivt programmering med Fish Shell](https://medium.com/@oschvr/effektivt-programmering-med-fish-shell-3f702d11bafc)
- [6 anledningar att använda Fish Shell](https://dev.to/agnithotr/6-reasons-to-use-fish-shell-551l)