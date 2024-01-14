---
title:    "Bash: Écrire des tests"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

Écrire des tests dans un programme Bash peut sembler fastidieux, mais c'est en fait une étape essentielle pour garantir la qualité et la fiabilité de votre code. En écrivant des tests, vous pouvez vérifier si votre code fonctionne comme prévu et détecter des erreurs potentielles avant de les déployer en production. Cela permet également de faciliter la maintenance et les mises à jour ultérieures du code.

## Comment faire

Pour écrire des tests dans un programme Bash, il existe trois techniques principales : les tests automatisés avec des commandes intégrées, l'utilisation de fichiers de test et l'utilisation d'un outil dédié comme Bash Automated Testing System (BATS).

### Tests automatisés avec des commandes intégrées

La manière la plus simple et la plus courante d'écrire des tests dans un programme Bash est d'utiliser les commandes intégrées telles que `test` ou `[[ ]]`. Ces commandes permettent de vérifier des conditions et de retourner un code de sortie en fonction du résultat du test. Par exemple, le test `[[ "$var" == "hello" ]]` vérifiera si la variable `var` est égale à "hello" et retournera un code de sortie de 0 (succès) si c'est le cas ou 1 (échec) sinon.

### Utilisation de fichiers de test

Une autre technique consiste à créer des fichiers de test séparés contenant une série de commandes Bash à exécuter pour vérifier la sortie de votre programme. Ces fichiers de test peuvent être exécutés manuellement ou automatiquement à l'aide d'un outil tel que `xtrace` ou `expect`.

### Utilisation de BATS

Bash Automated Testing System (BATS) est un outil spécialement conçu pour faciliter l'écriture de tests dans des programmes Bash. Il utilise des tests au format `tap` (Test Anything Protocol) et permet une gestion plus avancée des rapports de test et des erreurs. Pour utiliser BATS, il suffit d'installer l'outil et d'écrire vos tests dans un fichier `*.bats` en utilisant une syntaxe spécifique.

Voici un exemple d'un fichier de test BATS :

```
## Test unitaire pour la fonction de multiplication
@test "Multiplication de 4 par 2 devrait être égal à 8" {
    run multiplication 4 2
    [[ $status -eq 0 ]]
    [[ $output == "8" ]]
}

@test "Multiplication de 7 par 0 devrait être égal à 0" {
    run multiplication 7 0
    [[ $status -eq 0 ]]
    [[ $output == "0" ]]
}
```

Ce fichier contient deux tests pour une fonction de multiplication qui prend deux nombres en entrée et retourne leur produit. Le code `run` exécute la fonction avec les arguments fournis et stocke le code de sortie dans la variable `$status` et la sortie de la fonction dans la variable `$output`. Les commandes `[[ ]]` sont utilisées pour vérifier si le code de sortie et la sortie correspondent aux valeurs attendues.

## Plongée en profondeur

Pour écrire des tests efficaces dans un programme Bash, il est important de comprendre les différentes techniques de test et leurs limites. Par exemple, bien que les tests automatisés avec des commandes intégrées soient faciles à mettre en place, ils sont limités en termes de complexité et peuvent manquer de fiabilité dans certains cas. Les fichiers de test offrent une plus grande flexibilité, mais peuvent être plus difficiles à maintenir. L'utilisation de BATS peut sembler plus complexe au départ, mais elle offre des fonctionnalités plus avancées et une meilleure gestion des rapports de test.

## Voir aussi

- [Guide de démarrage pour écrire des tests dans un programme Bash](https://www.linux.com/training-tutorials/writing-bash-tests/)
- [Documentation officielle de BATS](https://github.com/sstephenson/bats)
- [Comment écrire des tests de qualité dans un programme Bash](https://www.shellcheck.net/blog/category_linux.html)