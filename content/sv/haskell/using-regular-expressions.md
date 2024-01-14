---
title:    "Haskell: Användning av regelbundna uttryck"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Varför?

Det finns många anledningar till varför programmerare kan använda sig av reguljära uttryck i Haskell. Det kan vara ett kraftfullt sätt att söka och manipulera text, och det kan också bidra till mer läsbar och effektiv kod.

# Hur man använder reguljära uttryck i Haskell

För att använda reguljära uttryck i Haskell behöver du importera modulen `Text.Regex`, som innehåller olika funktioner för att hantera uttryck. Sedan kan du skriva dina uttryck inuti en `Regex`-typ, och använda funktioner som `matchRegex` och `subRegex` för att matcha och modifiera texten.

```Haskell
import Text.Regex

-- Skapa en Regex-variabel med hjälp av "mkRegex" funktionen
myRegex :: Regex
myRegex = mkRegex "haskell"

-- Använd "matchRegex" för att hitta matchningar i en sträng
matchRegex myRegex "Jag älskar att programmera i Haskell"
-- "Just found a match for: "haskell" in text"

-- Använd "subRegex" för att ersätta matchningar med annan text
subRegex myRegex "Haskell är det bästa programsspråket" "Go är det bästa programmsspråket"
-- "Go är det bästa programmsspråket är det bästa programsspråket"
```

# Utforska mer

Det finns många olika funktioner och mönster som kan användas för att skapa kraftfulla reguljära uttryck i Haskell. Det är också viktigt att förstå hur man använder metatecken och grupperingar för att få ut det mesta av dessa uttryck. Det finns också andra användbara moduler som `Text.Regex.Posix` och `Text.Regex.PCRE` för mer avancerad användning.

# Se också

- [Haskell.org - Regulära uttryck](https://www.haskell.org/haskellwiki/Regular_expressions)
- [Haskell Language Report - Text.Regex](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17300010.2.1)
- [Haskell Cookbook - Reguljära uttryck](https://haskellcookbook.com/chapters/strings.html#regexpr)