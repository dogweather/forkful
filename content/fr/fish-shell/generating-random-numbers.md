---
title:                "Production de nombres aléatoires"
html_title:           "Fish Shell: Production de nombres aléatoires"
simple_title:         "Production de nombres aléatoires"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un programmeur ou un amateur de la ligne de commande (command line), il est fort probable que vous ayez besoin de générer des nombres aléatoires à un moment donné. Peut-être pour tester un code, simuler un scénario ou même pour des jeux. Dans cet article, nous allons explorer comment générer des nombres aléatoires en utilisant le langage de shell Fish.

## Comment faire 

Nous allons utiliser la fonction `math` intégrée à Fish pour générer des nombres aléatoires. Voici un exemple de code simple pour générer un nombre aléatoire entre 1 et 10 :

````Fish Shell
math $RANDOM % 10 + 1
````

Le résultat de ce code sera un nombre aléatoire compris entre 1 et 10 (inclus). Vous pouvez modifier les valeurs pour obtenir un intervalle différent selon vos besoins. Voici un exemple de code pour générer un nombre aléatoire entre 100 et 200 :

````Fish Shell
math $RANDOM % 101 + 100
````

Vous pouvez également utiliser cette fonction pour générer plusieurs nombres aléatoires en utilisant une boucle for. Par exemple, si vous voulez générer 10 nombres aléatoires entre 1 et 10 :

````Fish Shell
for i in (seq 1 10)
    math $RANDOM % 10 + 1
end
````

Cela générera 10 nombres aléatoires différents, chacun compris entre 1 et 10.

## Plongée en profondeur 

La fonction `math` utilise le générateur de nombres pseudo-aléatoires C, qui est un générateur assez simple mais suffisamment bon pour la plupart des cas d'utilisation. Si vous avez besoin de générer des nombres plus complexes ou pour des cas d'utilisation plus avancés, vous pouvez utiliser des modules externes comme `random` ou `numpy`.

## Voir aussi 

- [Documentation officielle sur `math` dans Fish](https://fishshell.com/docs/current/cmds/math.html)
- [Documentation sur les modules externes `random` et `numpy`](https://pypi.org/project/random/)