---
date: 2024-01-20 17:50:21.085085-07:00
description: "How to: (S\xE5 h\xE4r g\xF6r du:) Str\xE4nginterpolering \xE4r ingen\
  \ nyhet i programmering. Det har anv\xE4nts i olika spr\xE5k, som Perl och PHP,\
  \ l\xE5ngt innan Bash\u2026"
lastmod: '2024-04-05T22:50:52.371970-06:00'
model: gpt-4-1106-preview
summary: "(S\xE5 h\xE4r g\xF6r du:) Str\xE4nginterpolering \xE4r ingen nyhet i programmering."
title: "Interpolera en str\xE4ng"
weight: 8
---

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
