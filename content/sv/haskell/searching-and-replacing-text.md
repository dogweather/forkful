---
title:                "Haskell: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en viktig del av programmering. Genom att lära sig hur man utför denna handling i Haskell kan du effektivisera ditt arbete och göra din kod mer läsbar.

## Hur man gör det

För att söka och ersätta text i Haskell kan du använda funktionen `replace` från paketet `text`. Du behöver först importera detta paket med hjälp av kommandot `import qualified Data.Text as T`, vilket ger dig möjlighet att använda prefixet `T` för alla funktioner i paketet.

```Haskell
import qualified Data.Text as T

-- En enkel sträng att arbeta med
let str = "Detta är en textsträng för exempel."

-- Söker efter text och ersätter den med ny text
let nyStr = T.replace "text" "textsträng" str

-- Skriver ut resultatet
print nyStr
```

Output: `Detta är en textsträngsträng för exempel.`

Observera att i Haskell är alla strängar representerade som värden av typen `Text` från paketet `text`, och inte som `String` som i många andra programmeringsspråk.

## Djupdykning

För att förstå hur funktionen `replace` fungerar, låt oss titta på dess signatur:
```Haskell
replace :: Text -> Text -> Text -> Text
```

Detta betyder att funktionen tar tre strängar som argument och returnerar en sträng. Det första argumentet är den text som ska sökas efter, det andra är den text som ska ersätta den och det tredje är den ursprungliga texten. Om det finns flera matchningar av den text som ska bytas ut, kommer alla dessa att ersättas.

En viktig sak att notera är att funktionen `replace` är strängen anpassad och tar hänsyn till versaler och gemener i sin sökning.

## Se även (See Also)

- [Officiell dokumentation för paketet `text`](https://hackage.haskell.org/package/text)
- [En introduktion till Haskell för nybörjare](https://en.wikipedia.org/wiki/Haskell_(programming_language))
- [Lär dig mer om vanliga strängoperationer i Haskell](https://haskell-lang.org/tutorial/strings)