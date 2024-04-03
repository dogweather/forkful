---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:42.807366-07:00
description: "Comment faire : Fish n'a pas de cadre de test int\xE9gr\xE9 comme certains\
  \ autres environnements de programmation. Cependant, vous pouvez \xE9crire des scripts\
  \ de\u2026"
lastmod: '2024-03-13T22:44:58.328436-06:00'
model: gpt-4-0125-preview
summary: "Fish n'a pas de cadre de test int\xE9gr\xE9 comme certains autres environnements\
  \ de programmation."
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :
Fish n'a pas de cadre de test intégré comme certains autres environnements de programmation. Cependant, vous pouvez écrire des scripts de test simples qui utilisent des assertions pour vérifier le comportement de vos fonctions. De plus, vous pouvez tirer parti d'outils tiers comme `fishtape` pour une suite de tests plus complète.

### Exemple 1 : Script de Test de Base
Commençons par une fonction basique en Fish qui calcule la somme de deux nombres :

```fish
function add --description 'Additionne deux nombres'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

Vous pouvez écrire un script de test basique pour cette fonction comme suit :

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add réussi"
    else
        echo "test_add échoué"
    end
end

test_add
```

L'exécution de ce script produirait :

```
test_add réussi
```

### Exemple 2 : Utilisation de Fishtape
Pour une solution de test plus robuste, vous pouvez utiliser `fishtape`, un exécuteur de test produisant du TAP pour Fish.

D'abord, installez `fishtape` si ce n'est pas déjà fait :

```fish
fisher install jorgebucaran/fishtape
```

Ensuite, créez un fichier de test pour votre fonction `add`, par exemple, `add_test.fish` :

```fish
test "Ajouter 3 et 4 donne 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

Pour exécuter le test, utilisez la commande suivante :

```fish
fishtape add_test.fish
```

Un exemple de sortie pourrait ressembler à ceci :

```
TAP version 13
# Ajouter 3 et 4 donne 7
ok 1 - test_add réussi
```

Cela vous indique que le test a réussi avec succès. `fishtape` vous permet de structurer des tests plus détaillés et fournit une sortie informative, facilitant le débogage et une couverture de test complète pour vos scripts Fish.
