---
title:    "Elm: Radera tecken som matchar ett mönster"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Varför

Att ta bort tecken som matchar ett mönster kan göra det enklare att hantera och bearbeta strängar i dina Elm-program. Detta kan komma till nytta om du till exempel vill filtrera bort vissa tecken eller byta ut dem med andra.

# Så här gör du

För att ta bort tecken som matchar ett visst mönster kan du använda funktionen `String.filter` tillsammans med en lambda-funktion som tar emot ett tecken som argument och returnerar `True` om det ska behållas eller `False` om det ska tas bort.

```Elm
sträng = "Detta är en sträng som behöver bearbetas"
mönster = 'i'
resultat = String.filter (\tecken -> tecken /= mönster) sträng
```

Resultatet blir en sträng där alla förekomster av bokstaven 'i' har tagits bort. Om du vill byta ut tecknet kan du använda funktionen `String.replace` tillsammans med `String.filter`.

```Elm
sträng = "Detta är en sträng som behöver bearbetas"
mönster = 'ä'
ersättMed = 'a'
resultat = String.replace mönster (String.fromChar ersättMed) (String.filter (\tecken -> tecken /= mönster) sträng)
```

Resultatet blir då "Datta ar an strang som behover bearbetas", där alla 'ä' har ersatts med 'a'.

# Djupdykning

I Elm finns också möjligheten att använda en reguljär uttryck (RegExp) för att matcha mönster och ta bort tecken. Detta kan vara särskilt användbart om du behöver ta bort flera olika tecken eller vill använda mer avancerade mönster.

```Elm
import RegExp

sträng = "123-456-789"
mönster = RegExp.make "[0-9-]"
resultat = String.filter (not << RegExp.contains mönster) sträng
```

Resultatet blir då strängen " " (en tom sträng) där både siffrorna och bindestrecken har tagits bort. Du kan även använda en reguljär uttryck för att byta ut tecken, genom att använda funktionen `String.replace` tillsammans med `RegExp.replace`.

# Se även

- [Elm dokumentation om String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Reguljära uttryck i Elm](https://dev.to/fbonetti/how-to-implement-regular-expressions-in-elm-32m5)
- [Regelexempel och övningar](https://regex-101.com/)