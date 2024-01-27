---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Utiliser des expressions régulières, c'est chercher et manipuler du texte avec des motifs précis. Les programmeurs s'en servent pour valider, trouver, remplacer ou diviser des données textuelles facilement.

## How to:
Lua ne supporte pas les expressions régulières comme Perl ou Python. Il utilise ses propres motifs. Voici quelques exemples :

```Lua
-- Recherche de motif
local texte = "Bonjour le monde!"
if string.find(texte, "monde") then
  print("Motif trouvé !")
end
-- Output: Motif trouvé !

-- Remplacement de texte
local remplacé = string.gsub(texte, "Bonjour", "Salut")
print(remplacé)
-- Output: Salut le monde!

-- Recherche de patterns complexes
local emails = "nom@domaine.com autre@mail.fr"
for email in emails:gmatch("[A-Za-z0-9%.]+@%a+%.[a-z]+") do
  print(email)
end
-- Output: nom@domaine.com
-- Output: autre@mail.fr
```

## Deep Dive
Les motifs Lua s'inspirent du standard POSIX et des expressions régulières de Perl, mais plus simples. Pour des motifs avancés, on utilise `lrexlib` ou `LPeg`. `lrexlib` s'appuie sur des librairies regex comme PCRE. `LPeg` propose une alternative puissante avec des parsing expressions grammars.

## See Also
- Documentation Lua sur les motifs : https://www.lua.org/manual/5.4/manual.html#6.4.1
- `LPeg`, une bibliothèque pour des motifs avancés : http://www.inf.puc-rio.br/~roberto/lpeg/
- `lrexlib`, pour des regex comme dans d’autres langages : https://github.com/rrthomas/lrexlib

Cet article a donné une vue d'ensemble rapide. Pour approfondir, consultez les liens. Bon codage !
