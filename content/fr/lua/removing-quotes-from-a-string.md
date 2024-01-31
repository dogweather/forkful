---
title:                "Retirer les guillemets d'une chaîne"
date:                  2024-01-26T03:40:29.117268-07:00
model:                 gpt-4-0125-preview
simple_title:         "Retirer les guillemets d'une chaîne"

category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Retirer les guillemets d'une chaîne de caractères signifie enlever ces caractères de guillemets doubles ou simples qui enlacent votre texte. Les programmeurs font cela pour assainir les entrées, simplifier l'analyse syntaxique, ou harmoniser des données qui pourraient être citées de manière inconsistante.

## Comment faire :
Voici comment éliminer ces guillemets dans Lua :

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Bonjour, Monde!"'))     -- Bonjour, Monde!
print(remove_quotes("'Au revoir, Guillemets!'"))  -- Au revoir, Guillemets!
```

Et voilà ! Ces guillemets ont disparu comme des chaussettes dans un sèche-linge.

## Analyse approfondie
Les gens enlèvent les guillemets des chaînes de caractères depuis que les langages peuvent gérer le texte, ce qui est à peu près depuis toujours. Dans Lua, la fonction `gsub` fait le gros du travail, manipulant les motifs comme un scalpel pour exciser les guillemets. Des alternatives ? Bien sûr, vous pourriez utiliser les expressions régulières dans les langages qui les supportent, ou écrire votre propre boucle qui parcourt chaque caractère (ennuyeux, mais bon, c'est votre temps).

La correspondance de motifs de Lua vous donne la puissance d'une expérience quasi regex sans importer une bibliothèque entière. Le chapeau (`^`) et le signe dollar (`$`) correspondent, respectivement, au début et à la fin de la chaîne ; `%p` correspond à tout caractère de ponctuation. Après avoir éliminé la ponctuation en début et en fin, nous capturons tout le reste avec `(.*),` et remplaçons toute la correspondance avec ce groupe de capture en utilisant `" %1"`.

Souvenez-vous que la correspondance de motifs de Lua n'est pas aussi puissante que les moteurs regex complets – par exemple, elle ne peut pas compter ou revenir en arrière. Cette simplicité est à la fois une bénédiction et une malédiction, selon les guillemets que vous manipulez et où ils se cachent.

## Voir aussi
Plongez plus profondément dans la correspondance de motifs de Lua avec le livre PiL (Programmation en Lua) : http://www.lua.org/pil/20.2.html

Pour une élégance pure, découvrez comment d'autres langages s'y prennent en comparaison, en commençant par `str.strip` de Python : https://docs.python.org/3/library/stdtypes.html#str.strip
