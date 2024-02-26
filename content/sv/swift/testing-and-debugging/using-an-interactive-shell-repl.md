---
date: 2024-01-26 04:18:29.765702-07:00
description: "Att anv\xE4nda ett interaktivt skal, eller en L\xE4s-Utv\xE4rdera-Skriv\
  \ Ut-Loop (REPL), l\xE5ter dig koda interaktivt. Programmerare anv\xE4nder det f\xF6\
  r att snabbt testa\u2026"
lastmod: '2024-02-25T18:49:36.568668-07:00'
model: gpt-4-0125-preview
summary: "Att anv\xE4nda ett interaktivt skal, eller en L\xE4s-Utv\xE4rdera-Skriv\
  \ Ut-Loop (REPL), l\xE5ter dig koda interaktivt. Programmerare anv\xE4nder det f\xF6\
  r att snabbt testa\u2026"
title: "Anv\xE4nda en interaktiv skal (REPL)"
---

{{< edit_this_page >}}

## Vad & Varför?
Att använda ett interaktivt skal, eller en Läs-Utvärdera-Skriv Ut-Loop (REPL), låter dig koda interaktivt. Programmerare använder det för att snabbt testa Swift-snuttar, felsöka eller lära sig språket.

## Hur man gör:
Anropa REPL genom att öppna en terminal och köra `swift`. Skriv kod direkt och tryck på Enter för att köra den. Här är en smakprov:

```Swift
1> let greeting = "Hej, REPL!"
greeting: String = "Hej, REPL!"
2> print(greeting)
Hej, REPL!
```

Avsluta med `:quit` eller `Kontroll-D`.

## Djupdykning
REPL:s rötter går långt tillbaka till Lisp-tolkare på 60-talet. Swifts REPL sitter ovanpå LLVM, ett kraftfullt kompilatorramverk, som erbjuder mer än bara grundläggande tolkning - det är ett fullfjädrat verktyg med autokomplettering, felsökning och mer. REPL är bra för att lära sig eller skapa prototyper, men det är inte en fristående utvecklingsmiljö. Vissa föredrar att använda Playgrounds i Xcode för en mer grafisk, filbaserad metod, medan andra håller sig till traditionell skriptredigering och körning.

Under huven kompilerar Swifts REPL dynamiskt kod till maskinspråk och exekverar den, vilket är varför det är relativt snabbt. Det kan också komma åt alla kompilerade Swift-moduler, eller till och med C-bibliotek, vilket gör den ganska kraftfull. Observera dock att inte allt fungerar perfekt i REPL; vissa Swift-funktioner, särskilt de som kräver komplexa projektinställningar eller storyboard-filer, fungerar inte här.

## Se även
- [Swift.org - Komma igång](https://www.swift.org/getting-started/#using-the-repl)
- Apples [Introduktion till Xcode Playgrounds](https://developer.apple.com/videos/play/wwdc2014/408/)
- [LLVM-projektet](https://llvm.org/)
