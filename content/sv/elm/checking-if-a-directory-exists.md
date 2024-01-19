---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Checker om en katalog existerar är operationen för att kontrollera integriteten till en katalogs plats i ett datasystems filstruktur. Programmerare gör det här för att hindra felutlösande försök till att läsa eller skriv över en inte existerande katalog.

## Hur man gör:

Elm har inte en inbyggd kodblock för att verifiera om en katalog existerar eftersom Elm-kod körs i webbläsaren och inte har permission att läsa filsystemet direkt. Men vi kan använda JavaScript's inbyggda ```fs``` paket genom att utrusta oss med några flags. Notera att FS modulen är en del av Node.js, inte webbläsarbaserad JavaScript.

```Elm
port checkDirectory : String -> Cmd msg
port directoryExists : (Bool -> msg) -> Sub msg
```
Observera: I Elm, ports är den godkända vägen för tvåvägs interaktion mellan Elm och JavaScript.

## Fördjupning:

Historiskt sett, frågan om hur att verifiera om en katalog existerar har förändrats med språkets utveckling och dess förmåga att interagera med operativsystemet. I den tidiga C och Java, var detta ett vanligt problem. 

Alternativ till Elm för att hantera sådana filsystemrelaterade operationer inkluderar användning av server-side språk som Python eller Ruby, eller att använda JavaScript direkt. 

Implementeringsdetaljer gäller begränsningarna i att använda webbläsarbaserade språk för direkt interaktion med filsystem. Elm kör i webbläsaren och är därmed begränsad i sin förmåga att direkt interagera med filsystemets operationer, så som att kontrollera om en katalog existerar.

## Se även:

1. Elm Officiella Dokument [Elm Language Docs](http://elm-lang.se/)
2. More on Ports in Elm [Ports in Elm](http://elm-lang.se/tutorial/external-commands-and-subscriptions)
3. Node.js's fs package documentation [Node.js's fs](https://nodejs.org/api/fs.html)
4. Learning more about JavaScript [Mozilla JavaScript Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
5. More about filesystem operations in JavaScript [Node.js File System](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)