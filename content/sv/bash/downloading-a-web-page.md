---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Att ladda ner en webbsida kan vara användbart av flera anledningar. Du kanske vill spara en sida för senare läsning eller använda dess innehåll i ditt eget projekt.

## Så här gör du

Ladda ner en webbsida med hjälp av Bash är enkelt. Först behöver du välja en lämplig URL som du vill ladda ner. Sedan kan du använda kommandot `wget` följt av URL-en i ett terminalfönster.

```Bash
wget https://www.example.com
```

Detta kommando kommer att ladda ner hela webbsidan till din nuvarande arbetskatalog. Om du vill ladda ner en specifik fil från webbsidan, kan du använda flaggan `-O` efter URL-en och ange önskat filnamn.

```Bash
wget https://www.example.com/image.jpg -O image.jpg
```

Du kan också använda flaggan `-P` för att ange en specifik sökväg för var du vill spara filen.

```Bash
wget https://www.example.com/image.jpg -P /hem/din_användare/bilder/
```

## Djupdykning

Bash kommandot `wget` används för att hämta filer från webben. Det fungerar genom att skicka en begäran till den angivna URL-en och hämta innehållet till din lokala enhet. Detta gör det möjligt att ladda ner filer, bilder och hela webbsidor.

En av de mest användbara funktionerna i `wget` är dess möjlighet att återuppta avbrutna nedladdningar. Om din nedladdning blir avbruten av någon anledning, till exempel dålig internetanslutning, kan du använda flaggan `-c` för att fortsätta nedladdningen från där den slutade.

Du kan också använda kommandot `curl` för att ladda ner webbsidor i Bash. Det fungerar på ett liknande sätt som `wget` och ger dig möjlighet att hämta innehållet från en URL.

## Se även

- [wget man page](https://www.gnu.org/software/wget/manual/wget.html)
- [curl man page](https://curl.se/docs/manpage.html)