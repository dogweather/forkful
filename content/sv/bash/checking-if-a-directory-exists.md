---
title:    "Bash: Kontrollera om en mapp existerar"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en katalog finns är en nyttig kunskap för alla som arbetar med Bash-programmering. Det kan hjälpa till att undvika fel och effektivisera programmet.

## Hur man gör det
För att kontrollera om en katalog finns i Bash-programmering, kan vi använda kommandot "test" med flaggan -d, som står för "directory". Detta ser ut såhär:

```Bash
if test -d katalogen; then
    echo "Katalogen finns!"
fi
```

I detta exempel behöver vi inte ens använda variabeln "katalogen", utan kan ersätta den med en direkt sökväg till katalogen vi vill kontrollera. Om katalogen finns, kommer programmet att skriva ut "Katalogen finns!".

## Djupdykning
En djupare förståelse för detta koncept innebär att förstå hur flaggan -d fungerar. Den kollar inte bara om namnet på katalogen är exakt som den ges i kommandot, utan den kollar också om det faktiskt är en katalog och inte en fil eller något annat. Detta gör att vi kan vara säkra på att vi endast kollar efter kataloger.

Det är också viktigt att förstå att test-kommandot kan ha andra flaggor för att kontrollera andra variabler, till exempel för att kolla om en fil finns eller om en variabel är satt.

## Se också
- [Bash-testkommandot](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html#Bash-Conditional-Expressions)
- [Bash-testkommandots flaggor](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html#Shell-Conditional-Expressions)