---
title:                "Interpolera en sträng"
date:                  2024-01-20T17:50:48.153851-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolering av en sträng innebär att du dynamiskt stoppar in data i en sträng. Programmerare gör detta för att skräddarsy meddelanden, skapa dynamiska kommandon eller hantera data mer flexibelt.

## How to:
```Fish Shell
set name "Världsmedborgare"
echo "Hej, $name! Hur mår du idag?"

# Output: Hej, Världsmedborgare! Hur mår du idag?
```

```Fish Shell
set count 5
echo "Du har $count nya meddelanden."

# Output: Du har 5 nya meddelanden.
```

```Fish Shell
set user "Kalle"
set dir "/home/$user/projects"
echo "Dina projekt ligger i katalogen: $dir"

# Output: Dina projekt ligger i katalogen: /home/Kalle/projects
```

## Deep Dive
Interpolering av strängar har sina rötter i tidiga skriptspråk där behovet av att enkelt sätta in variabler i textsträngar blev uppenbart. I Fish Shell sker detta genom att direkt ange variabeln i strängen med ett `$`-tecken. Alternativ till Fish Shell för stränginterpolering inkluderar bash och zsh, där syntaxen kan variera något. Fish utför interpolering i alla dubbelt citerade strängar, men inte i enkelt citerade strängar, vilket skyddar mot oavsiktlig expansion. Detta är ett designval för att förenkla och säkra användningen av strängar i shellskript.

## See Also
- Fish Shell dokumentation om variabler: https://fishshell.com/docs/current/index.html#variables
- En guide till Fish Shell skript: https://fishshell.com/docs/current/tutorial.html#tut_scripting
