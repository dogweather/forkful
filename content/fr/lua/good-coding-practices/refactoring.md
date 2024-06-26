---
date: 2024-01-26 01:45:47.099488-07:00
description: "Comment faire : Prenons une simple fonction Lua et refactorisons-la.\
  \ Nous commen\xE7ons avec une fonction qui calcule la somme des nombres dans une\
  \ liste\u2026"
lastmod: '2024-03-13T22:44:57.945621-06:00'
model: gpt-4-0125-preview
summary: Prenons une simple fonction Lua et refactorisons-la.
title: 'Refactoring : Mode d''emploi'
weight: 19
---

## Comment faire :
Prenons une simple fonction Lua et refactorisons-la. Nous commençons avec une fonction qui calcule la somme des nombres dans une liste mais est écrite sans beaucoup de souci pour l'efficacité ou la clarté :

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- Affiche : 10
```

Refactoriser pour une version plus efficace et lisible :
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- Affiche toujours : 10
```

La version refactorisée supprime la boucle intérieure redondante, en utilisant `ipairs` pour parcourir la liste de manière propre.

## Plongée profonde
Historiquement, le refactoring provient de la communauté de programmation Smalltalk à la fin des années 80 et a été popularisé par le livre de Martin Fowler 'Refactoring: Improving the Design of Existing Code'. En Lua, refactoriser implique souvent de simplifier les conditionnelles complexes, de décomposer les grandes fonctions en fonctions plus petites, et d'optimiser l'utilisation des tables pour améliorer la performance.

Refactoriser en Lua a ses mises en garde ; la nature dynamique de Lua et le typage flexible peuvent rendre certains refactorings, comme renommer des variables ou changer les signatures de fonction, plus risqués si ce n'est pas fait avec prudence. Les outils d'analyse de code statique (comme `luacheck`) peuvent atténuer ces risques. Les alternatives incluent le développement piloté par les tests (TDD), où le code est continuellement refactorisé comme une partie intégrante du processus de développement, contrairement à une phase de refactoring séparée.

## Voir aussi
- "Programming in Lua" par Roberto Ierusalimschy pour les meilleures pratiques et exemples.
- "Refactoring: Improving the Design of Existing Code" par Martin Fowler pour les principes applicables à travers les langages.
- Annuaire LuaRocks (https://luarocks.org/) pour les outils et modules visant à maintenir et refactoriser le code Lua.
