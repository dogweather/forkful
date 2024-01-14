---
title:    "Elm: Söka och ersätta text"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Varför

Det kan ibland vara frustrerande och tidskrävande att manuellt söka och ersätta text i sin kod. Med hjälp av Elm och dess inbyggda funktioner kan denna process göras enklare och snabbare. 

# Hur man gör det

För att söka och ersätta text i Elm, använd funktionen `replace` som tar emot tre argument: den ursprungliga strängen, den sökta strängen och den ersättningssträngen. Nedan ser du en kodexempel som ersätter alla förekomster av "hej" med "hello" i en sträng:

```Elm
replace "hej everyone" "hej" "hello" == "hello everyone"
```

Obs: den här funktionen returnerar en ny sträng, den ändrar inte den ursprungliga strängen.

# Dyk ner i detaljer

För att göra sök- och ersättningsprocessen mer robust kan man använda sig av Elm-paketet `elm-string-extra` som erbjuder flera användbara funktioner, såsom att ignorera eller matcha stora och små bokstäver och använda reguljära uttryck. Se nedan för exempel av dessa funktioner:

```Elm
import String.Extra exposing (replaceCaseInsensitive, replaceAllRegex)

replaceCaseInsensitive "This is a TeSt" "test" "example" == "This is a example"
replaceAllRegex "[0-9]+" "There are 10 apples and 15 oranges." "X" == "There are X apples and X oranges."
```

# Se även

- [Elm dokumentation för strängar](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm-paketet elm-string-extra på Elm Packages](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
- [RegExr - en användbar plattform för att testa reguljära uttryck](https://regexr.com/)