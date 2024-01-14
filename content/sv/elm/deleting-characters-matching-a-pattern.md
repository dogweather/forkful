---
title:                "Elm: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Varför

Att ta bort tecken som matchar ett mönster kan vara ett användbart verktyg för att manipulera strängar i din Elm-kod. Det kan hjälpa dig att filtrera ut oönskade tecken eller byta ut dem mot andra.

# Så här gör du

För att ta bort tecken som matchar ett visst mönster i Elm, kan du använda funktionen `String.strip` tillsammans med `Regex.replace`. Här är ett exempel på hur du kan använda detta för att ta bort alla siffror från en sträng:

```Elm
import String
import Regex

sträng = "Jag är 25 år gammal"
färdigSträng =
    String.strip (Regex.replace Regex.All "\d" (\_ -> "") sträng)
-- Resultatet blir "Jag är år gammal"
```

Förklaring:
1. Importera modulen `String` och `Regex` för att kunna använda deras funktioner i din kod.
2. Skapa en sträng som du vill ta bort tecken från.
3. Använd funktionen `Regex.replace` tillsammans med `String.strip` för att filtrera ut alla siffror från strängen.
4. Det första argumentet för `Regex.replace` är `Regex.All`, vilket betyder att vi vill matcha alla förekomster av mönstret.
5. Det andra argumentet är mönstret vi vill matcha, i det här fallet `"\d"` som står för en siffra.
6. Det sista argumentet är en funktion som avgör vad som ska ersätta de matchande tecknen. I vårt fall vill vi bara ha en tom sträng, så vi använder lambdafunktionen `(\_ -> "")`.
7. Slutligen använder vi `String.strip` för att ta bort eventuella mellanslag som lämnats kvar efter borttagningen av siffrorna.

# Djupdykning

Det finns många olika sätt att använda funktionen `Regex.replace` för att ta bort tecken som matchar ett mönster. Du kan till exempel använda mer komplexa mönster för att filtrera ut flera olika tecken på en gång. Det finns också andra användbara funktioner för strängmanipulering i Elm, som `String.split` och `String.repeat`.

# Se också

- Elm guide om strängmanipulering: https://guide.elm-lang.org/strings/
- Dokumentation för `String`-modulen: https://package.elm-lang.org/packages/elm/core/latest/String