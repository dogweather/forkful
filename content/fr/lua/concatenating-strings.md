---
title:                "Concaténation de chaînes de caractères"
html_title:           "Lua: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs le font?

La concaténation de chaînes est l'action de fusionner plusieurs chaînes de caractères en une seule plus longue. Les programmeurs le font souvent pour faciliter la manipulation de données ou pour créer des messages dynamiques pour les utilisateurs.

## Comment faire:

Voici un exemple de code en Lua pour concaténer deux chaînes de caractères:

```Lua
nom = "Marie"
prenom = "Antoine"
print(nom .. " " .. prenom)

-- Output: Marie Antoine
```

Vous pouvez également utiliser l'opérateur de concaténation `..` pour ajouter une valeur à une chaîne existante:

```Lua
message = "Bonjour "
nom = "Marie"
print(message .. nom)

-- Output: Bonjour Marie
```

## Approfondissement:

La concaténation de chaînes est une pratique courante dans la programmation depuis de nombreuses années. Cependant, certains langages de programmation offrent des fonctionnalités plus avancées pour manipuler les chaînes, comme la substitution de variables ou l'utilisation de modèles. Certains développeurs préfèrent également utiliser des bibliothèques externes pour faciliter la concaténation de chaînes.

En Lua, il est important de noter que la concaténation de chaînes peut entraîner une baisse de performance lorsqu'elle est utilisée en boucle, car chaque concaténation crée une nouvelle chaîne. Dans ce cas, il est préférable d'utiliser la fonction `table.concat()`.

## Voir aussi:

- La documentation officielle de Lua pour en savoir plus sur la concaténation de chaînes: https://www.lua.org/manual/5.4/manual.html#3.4.6
- Un article sur les différentes façons de concaténer des chaînes en Lua: https://coderwall.com/p/lx7jtq/lua-concatenate-strings-with-string-formatting-options