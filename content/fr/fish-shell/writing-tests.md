---
title:                "Écriture de tests"
html_title:           "Fish Shell: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?
Ecrire des tests est une pratique courante pour les programmeurs afin de vérifier la fiabilité et la stabilité de leur code. Cela consiste à créer des cas de test pour exercer différentes parties du code et s'assurer qu'elles fonctionnent comme prévu.

# Comment faire:
Voici un exemple simple pour créer et exécuter un test avec Fish Shell:

```Fish Shell
function add_numbers
  set result (math "$argv[1] + $argv[2]")
  echo $result
end

begin; and
  add_numbers 2 3
end

# Output: 5
```

Dans cet exemple, nous créons une fonction qui ajoute deux nombres et utilisons la commande "begin; and" pour exécuter le test. Si le résultat est correct, le test retournera "ok", sinon il affichera un message d'erreur.

# Plongée en profondeur:
L'écriture de tests a une longue histoire et a été popularisée par les pratiques de développement Agile et Extreme Programming. Bien qu'il existe d'autres outils pour écrire des tests, Fish Shell offre une syntaxe simple et facile d'utilisation.

Il existe également différentes approches pour écrire des tests, telles que les tests unitaires, les tests d'intégration et les tests fonctionnels. Choisissez celui qui correspond le mieux à vos besoins et à votre style de développement.

# Voir aussi:
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Automatiser avec Fish Shell](https://medium.com/@Dindaleon/automatiser-avec-fish-shell-9f61d3825a57)
- [History and Evolution of Writing Tests](https://medium.com/@martyav/writing-tests-a-primer-cea715c78e90)