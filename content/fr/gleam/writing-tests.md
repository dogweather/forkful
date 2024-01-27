---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire des tests, c'est vérifier que le code fait bien ce qu'il doit. On teste pour éviter les bugs, garantir la qualité et dormir tranquille.

## Comment faire :
```Gleam
// Définissons une fonction simple pour l'exemple
fn ajoute_deux(num: Int) -> Int {
  num + 2
}

// Et un test pour cette fonction
#[test]
fn test_ajoute_deux() {
  assert ajoute_deux(2) == 4
}
```
Sortie :
```
test test_ajoute_deux ... ok
```

## Plongée profonde :
Historiquement, les tests suivent les évolutions des langages de programmation, de simples assertions aux frameworks spécialisés. En Gleam, on privilégie la simplicité et la sécurité. Alternative : on peut aussi faire des tests de propriété avec des outils comme QuickCheck. Les tests en Gleam s'appuient sur le framework de test intégré, sans fioritures et efficace.

## Voir aussi :
- Article sur l'importance des tests en développement logiciel : [https://medium.com/@hudsonmendes/the-importance-of-software-testing-56f4baf0f5d8](https://medium.com/@hudsonmendes/the-importance-of-software-testing-56f4baf0f5d8)
