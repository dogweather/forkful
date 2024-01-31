---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
simple_title:         "Använda reguljära uttryck"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är mönster för att matcha textsträngar. Programmerare använder dem för att snabbt söka, ersätta och validera text.

## How to:
Haskell hanterar reguljära uttryck genom `regex`-paketet. Installera det med `cabal install regex-posix` om du inte redan har det. Här är några exempel:

```Haskell
import Text.Regex.Posix ((=~))

-- Hitta om text innehåller siffror
hittaSiffror :: String -> Bool
hittaSiffror text = text =~ "[0-9]" :: Bool

-- Exempel på användning
main = do
    print $ hittaSiffror "Detta är år 2023" -- Output: True
    print $ hittaSiffror "Inga siffror här" -- Output: False
```

## Deep Dive
Reguljära uttryck har använts sedan 1950-talet, ursprungligen i automatteori och formella språk. I Haskell, `regex-posix` är ett bibliotek som implementerar POSIX regex-interfacet, vilket ger portabilitet. Alternativen innefattar `regex-pcre` för Perl-stil regex. Detaljer i implementation inkluderar backtracking algoritmer och just-in-time kompilering för snabbsökningar.

## See Also
- Haskell Text.Regex.Posix-dokumentation: [hackage.haskell.org/package/regex-posix](https://hackage.haskell.org/package/regex-posix)
- Wiki om reguljära uttryck: [en.wikipedia.org/wiki/Regular_expression](https://en.wikipedia.org/wiki/Regular_expression)
