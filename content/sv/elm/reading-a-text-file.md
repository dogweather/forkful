---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa en textfil är en process där programmerare extraherar information lagrad i en fil. Det gör vi för att modifiera, analysera eller använda den informationen i våra program. 

## Så här gör du:

Tyvärr, i dagens version av Elm (0.19.1) har du inte direkt åtkomst till filsystemet. Du kan dock använda `ports` för att kommunicera med JavaScript.

```Elm
port module Main exposing (..)

port readFile : String -> Cmd msg

port fileRead : (String -> msg) -> Sub msg
```
I Elm kan du skicka efterfrågningen till JavaScript med `readFile` och lyssna på svaren med `fileRead`.

## Fördjupad Infomation:

Historiskt sett i Elm har direkt tillgång till filsystemet inte varit en prioritet då Elm primärt är utformat för webbapplikationer. 

Alternativ till Elm för filåtkomst kan vara Node.js eller Python, båda välkända för sina omfattande IO-funktioner. 

Om du vill tillhandahålla denna funktionalitet inom en Elm-applikation kan du implementera den genom JavaScript interop via “ports” vilket vi nämnde i hur-sektionen. 

## Se Även:

För att läsa mer om filhantering i Elm och JavaScript rekommenderar vi följande resurser:

1. [Elm Ports Dokumentation](https://guide.elm-lang.org/interop/ports.html)
2. [Node.js fs modul](https://nodejs.org/api/fs.html)
3. [JavaScript File API](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications)