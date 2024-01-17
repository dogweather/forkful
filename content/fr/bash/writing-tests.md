---
title:                "Écriture de tests"
html_title:           "Bash: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Ecrire des tests en programmation, c'est vérifier que notre code fonctionne correctement et qu'il ne contient pas d'erreurs. Les programmeurs le font pour s'assurer que leur code fonctionne tel qu'ils l'ont prévu et pour éviter les bugs dans leurs programmes.

## Comment faire:
Voici comment écrire des tests dans Bash:

```Bash
# Définir une fonction pour tester
function addition {
  result=$(( $1 + $2 ))
  echo $result
}

# Appeler la fonction et stocker le résultat dans une variable
result=$(addition 3 5)

# Afficher le résultat attendu
echo $result
```

Résultat attendu: 8

## Plongez plus profondément:
L'utilisation de tests en programmation a gagné en popularité dans les années 1960 avec l'émergence des méthodes de développement logiciel telles que le TDD (Test Driven Development). Alternativement, certains programmeurs préfèrent utiliser des outils spécifiques pour les tests tels que PHPUnit ou Mocha. Dans Bash, nous pouvons utiliser des assertions pour vérifier si les résultats obtenus correspondent aux résultats attendus.

Pour implémenter des tests dans vos scripts Bash, vous pouvez utiliser des outils comme BATS (Bash Automated Testing System) ou Shunit2 pour écrire des tests unitaires automatisés.

## Voir aussi:
Vous pouvez en savoir plus sur la façon d'écrire des tests en Bash en consultant ces sources utiles:

- [BASH Programming - Introduction HOW-TO](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-2.html)
- [BATS Documentation](https://github.com/sstephenson/bats)
- [Shunit2 Documentation](https://github.com/kward/shunit2)