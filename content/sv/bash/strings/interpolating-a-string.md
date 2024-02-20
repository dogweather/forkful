---
date: 2024-01-20 17:50:21.085085-07:00
description: "Interpolering av str\xE4ngar till\xE5ter dig att infoga variabler eller\
  \ uttryck i en str\xE4ng. Programmerare anv\xE4nder det f\xF6r att skapa dynamiska\
  \ meddelanden och\u2026"
lastmod: 2024-02-19 22:04:57.295372
model: gpt-4-1106-preview
summary: "Interpolering av str\xE4ngar till\xE5ter dig att infoga variabler eller\
  \ uttryck i en str\xE4ng. Programmerare anv\xE4nder det f\xF6r att skapa dynamiska\
  \ meddelanden och\u2026"
title: "Interpolera en str\xE4ng"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Interpolering av strängar tillåter dig att infoga variabler eller uttryck i en sträng. Programmerare använder det för att skapa dynamiska meddelanden och för att förenkla sammanfogning av text och data.

## How to: (Så här gör du:)
```Bash
name="Världen"
greeting="Hej, $name!"
echo $greeting  # Skriver ut: Hej, Världen!

# Med kommandoutbyte:
user_count=$(who | wc -l)
echo "Det är $user_count användare inloggade på systemet."

# Med aritmetik:
a=5
b=7
echo "$(( a + b )) är summan av 5 och 7."  # Skriver ut: 12 är summan av 5 och 7.
```

## Deep Dive (Djupdykning)
Stränginterpolering är ingen nyhet i programmering. Det har använts i olika språk, som Perl och PHP, långt innan Bash introducerade det. I Bash ersätter `$variabelnamn` eller `${variabelnamn}` med värdet av variabeln, och `$(kommando)` tar utdatan från ett kommando. Alternativ som `'single quotes'` förhindrar interpolering, bra när exakta värden behövs. I komplicerade skript, använd dubbla citattecken för att undvika oönskad ordexpansion. Kom ihåg, allt i Bash er processas sekventiellt, så interpolering sker vid exekveringstid.

## See Also (Se även)
- [Bash Variable Expansion](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)
- [Bash command substitution](https://www.gnu.org/software/bash/manual/bash.html#Command-Substitution)
