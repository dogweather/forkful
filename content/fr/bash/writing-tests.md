---
title:                "Rédaction de tests"
date:                  2024-02-03T19:29:47.061995-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rédaction de tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire des tests en Bash implique de scripter des cas de test pour valider la fonctionnalité de vos scripts Bash. Les programmeurs effectuent des tests pour s'assurer que leurs scripts fonctionnent comme prévu dans diverses conditions, capturant les erreurs et les bugs avant le déploiement.

## Comment faire :
Bash n'a pas de cadre de test intégré, mais vous pouvez écrire des fonctions de test simples. Pour des tests plus sophistiqués, des outils tiers comme `bats-core` sont populaires.

### Exemple de Test Basique en Bash Pur :
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "Test réussi."
    return 0
  else
    echo "Test échoué. Attendu '$expected_output', obtenu '$result'"
    return 1
  fi
}

# Invocation de la fonction de test
test_example_function
```
Sortie d'exemple :
```
Test réussi.
```

### Utilisation de `bats-core` pour les Tests :
Premièrement, installez `bats-core`. Cela peut généralement être fait via votre gestionnaire de paquets ou en clonant son dépôt.

Ensuite, écrivez vos tests dans des fichiers `.bats` séparés.

```bash
# Fichier : example_function.bats

#!/usr/bin/env bats

@test "tester l'exemple de fonction" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
Pour exécuter vos tests, exécutez simplement le fichier `.bats` :
```bash
bats example_function.bats
```
Sortie d'exemple :
```
 ✓ tester l'exemple de fonction

1 test, 0 échecs
```

Cette approche vous permet d'intégrer facilement les tests dans votre flux de travail de développement, garantissant la fiabilité et la stabilité de vos scripts Bash.
