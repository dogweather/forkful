---
title:    "Bash: Écriture des tests"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi
Ecrire des tests peut sembler fastidieux pour certains, mais cela peut faire la différence entre un code fonctionnel et un code bugué. En écrivant des tests, vous vous assurez que votre code fonctionne correctement et vous épargnez des heures de débogage par la suite.

## Comment faire
Ecrire des tests dans Bash peut sembler intimidant au premier abord, mais heureusement, le processus est assez simple. Vous pouvez utiliser des commandes telles que `echo` et `exit` pour contrôler le flux de votre script et vérifier si les résultats correspondent à ce que vous attendez.

Voici un exemple de script pour tester une fonction qui calcule la somme de deux nombres :

```Bash
#!/bin/bash

# Définir une fonction pour calculer la somme
function somme() {
    local somme=$(( $1 + $2 ))
    echo "La somme de $1 et $2 est égale à $somme."
}

# Appeler la fonction et stocker le résultat dans une variable
resultat=$(somme 5 7)

# Vérifier si le résultat est correct
if [ "$resultat" == "La somme de 5 et 7 est égale à 12." ]; then
    echo "Test réussi !"
else
    echo "Erreur : résultat inattendu."
fi
```

Lorsque vous exécutez ce script, vous devriez obtenir l'output suivant :

```
La somme de 5 et 7 est égale à 12.
Test réussi !
```

## Plongée en profondeur
Il existe plusieurs façons d'écrire des tests dans Bash, y compris l'utilisation de la commande `test` ou de l'opérateur `[[ ]]` pour vérifier des conditions. Vous pouvez également créer des fichiers de tests séparés pour les différentes parties de votre code.

Il est important de couvrir plusieurs scénarios possibles dans vos tests, en utilisant des valeurs extrêmes et en vérifiant les erreurs. Vous pouvez également utiliser des outils tels que `bash-test` pour automatiser le processus de création de tests.

N'oubliez pas que les tests doivent être écrits avec le même soin et la même attention que votre code, car ils font partie intégrante de votre processus de développement.

## Voir aussi
- [Bash Test Driven Development (TDD)](https://spin.atomicobject.com/2016/06/02/bash-tdd/)
- [Bash Testing Techniques](https://www.cloudbees.com/blog/advanced-bash-techniques-testing)
- [bash-test](https://github.com/bahamas10/bash-test)