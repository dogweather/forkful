---
date: 2024-01-26 00:55:27.363154-07:00
description: "G\xE9rer les erreurs en programmation, c'est anticiper l'impr\xE9vu.\
  \ C'est l'art de planifier pour quand les choses d\xE9rapent afin de pouvoir maintenir\
  \ votre\u2026"
lastmod: '2024-03-11T00:14:31.884285-06:00'
model: gpt-4-1106-preview
summary: "G\xE9rer les erreurs en programmation, c'est anticiper l'impr\xE9vu. C'est\
  \ l'art de planifier pour quand les choses d\xE9rapent afin de pouvoir maintenir\
  \ votre\u2026"
title: Gestion des erreurs
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Gérer les erreurs en programmation, c'est anticiper l'imprévu. C'est l'art de planifier pour quand les choses dérapent afin de pouvoir maintenir votre programme en fonctionnement fluide.

## Comment faire :
Lua utilise deux fonctions principales pour la gestion des erreurs : `pcall` et `xpcall`. Voici comment les utiliser :

```lua
function might_fail()
    if math.random() > 0.5 then
        error("Oups ! Quelque chose a mal tourné.")
    else
        print("Tout va bien !")
    end
end

-- Utilisation de pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("Succès !")
else
    print("Erreur capturée :", errorMessage)
end

-- Utilisation de xpcall avec un gestionnaire d'erreurs
function myErrorHandler(err)
    print("Le gestionnaire d'erreurs dit :", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("L'appel a-t-il réussi ?", status)
```

Un exemple de sortie pourrait être :

```
Erreur capturée : Oups ! Quelque chose a mal tourné.
Le gestionnaire d'erreurs dit : Oups ! Quelque chose a mal tourné.
L'appel a-t-il réussi ? false
```
Ou, si aucune erreur ne survient :
```
Tout va bien !
Succès !
Tout va bien !
L'appel a-t-il réussi ? true
```

## Plongée en profondeur
La gestion des erreurs, ou "gestion des exceptions", n'a pas toujours été d'actualité. Les premiers programmes plantaient – beaucoup. Avec l'évolution de la programmation, le besoin de stabilité est également apparu. L'approche de Lua est simple par rapport à certains langages. Il n'y a pas de blocs `try/catch`, juste `pcall` et `xpcall`. Le premier protège un appel de fonction, en retournant un statut et une erreur éventuelle. Le second ajoute une fonction de gestion d'erreurs, utile pour un nettoyage personnalisé ou la journalisation.

Une alternative dans Lua est d'utiliser `assert`, qui peut jouer un rôle similaire en générant une erreur si sa condition est fausse. Mais cela n'est pas aussi flexible que `pcall` pour des scénarios de gestion d'erreurs complexes.

En interne, `pcall` et `xpcall` fonctionnent en mettant en place un "environnement protégé" pour l'exécution de la fonction. Si une erreur survient, l'environnement la capte et peut soit la gérer immédiatement, soit la renvoyer pour que le programme la gère.

## Voir aussi
- Le livre "Programming in Lua" (troisième édition), disponible sur https://www.lua.org/pil/ pour une lecture approfondie de la gestion des erreurs (Section 8.4).
- Manuel de référence officiel de Lua 5.4 : https://www.lua.org/manual/5.4/ - pour les informations les plus à jour sur les fonctions de gestion des erreurs de Lua.
- Wiki des utilisateurs de Lua sur la gestion des erreurs : http://lua-users.org/wiki/ErrorHandling - pour des aperçus de la communauté et des modèles.
