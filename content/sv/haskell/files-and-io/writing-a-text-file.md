---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:09.676438-07:00
description: "Hur man g\xF6r: Haskells standard Prelude tillhandah\xE5ller grundl\xE4\
  ggande st\xF6d f\xF6r att skriva till filer med funktionerna `writeFile` och `appendFile`\
  \ fr\xE5n\u2026"
lastmod: '2024-03-13T22:44:37.973114-06:00'
model: gpt-4-0125-preview
summary: "Haskells standard Prelude tillhandah\xE5ller grundl\xE4ggande st\xF6d f\xF6\
  r att skriva till filer med funktionerna `writeFile` och `appendFile` fr\xE5n modulen\
  \ `System.IO`."
title: Att skriva en textfil
weight: 24
---

## Hur man gör:
Haskells standard Prelude tillhandahåller grundläggande stöd för att skriva till filer med funktionerna `writeFile` och `appendFile` från modulen `System.IO`. Här är ett grundläggande exempel på att skapa en ny fil (eller skriva över en befintlig) och sedan lägga till text i en fil.

```haskell
import System.IO

-- Skriver till en fil, skriver över om den existerar
main :: IO ()
main = do
  writeFile "example.txt" "Det här är rad ett.\n"
  appendFile "example.txt" "Det här är rad två.\n"
```

När du kör detta program skapas (eller rensas) `example.txt` och skriver "Det här är rad ett." följt av "Det här är rad två." på nästa rad.

För mer avancerad filhantering vänder sig Haskell-programmerare ofta till `text`-paketet för effektiv stränghantering och `bytestring`-paketet för att hantera binärdata. Så här använder du `text`-paketet för fil-I/O:

Först behöver du lägga till `text` i ditt projekts beroenden. Sedan kan du använda det enligt följande:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Skriver till en fil med text-paketet
main :: IO ()
main = do
  let innehåll = T.pack "Använder text-paketet för bättre prestanda.\n"
  TIO.writeFile "textExample.txt" innehåll
  TIO.appendFile "textExample.txt" $ T.pack "Lägger till rad två.\n"
```

I detta kodsnutt konverterar `T.pack` en vanlig `String` till `Text`-typen, vilket är mer effektivt. `TIO.writeFile` och `TIO.appendFile` är `text`-ekvivalenterna för att skriva till och lägga till i filer, respektive.

Kör du denna kod kommer resultatet att bli en fil med namnet `textExample.txt` med två rader text, vilket demonstrerar både skapande och läggande av förmågor med det avancerade `text`-biblioteket för bättre prestanda och kapacitet i hantering av Unicode-text.
