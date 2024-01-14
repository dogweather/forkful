---
title:    "Go: Écrire des tests"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi
L'écriture de tests est un aspect essentiel du développement logiciel en Go. En écrivant des tests, vous vous assurez que votre code fonctionne correctement et vous pouvez facilement le vérifier à l'avenir si des modifications sont apportées. Cela vous permet également de détecter rapidement et de résoudre les bugs avant qu'ils ne deviennent un problème majeur.

## Comment faire
L'écriture de tests en Go est simple et intégrée au processus de développement. Voici un exemple de code pour un test unitaire de la fonction « add » :

```Go
func add(x, y int) int {
  return x + y
}

func TestAdd(t *testing.T) {
  result := add(5, 10)
  expected := 15
  if result != expected {
    t.Errorf("Expected %d, but got %d", expected, result)
  }
}
```

Ce test utilise la fonction de test intégrée de Go, « Testing », pour vérifier si le résultat de la fonction « add » correspond à la valeur attendue. Si ce n'est pas le cas, le test échoue avec un message d'erreur.

## Plongée en profondeur
L'écriture de tests en Go va au-delà des simples tests unitaires. Vous pouvez également écrire des tests d'intégration, des tests de performance et des tests de charge pour garantir que votre code fonctionne dans toutes les situations.

De plus, Go dispose de nombreuses fonctionnalités pour faciliter l'écriture de tests, notamment l'utilisation de goroutines pour les tests parallèles, l'utilisation de benchmarks pour mesurer les performances, et la possibilité d'utiliser des outils tels que « go test -cover » pour vérifier la couverture des tests.

## Voir aussi
Pour en savoir plus sur l'écriture de tests en Go, consultez ces ressources :

- [Documentation officielle de Go sur les tests](https://golang.org/pkg/testing/)
- [Tutoriel sur l'écriture de tests en Go](https://medium.com/rungo/unit-testing-made-easy-in-go-25077669318)
- [Guide pratique pour écrire des tests en Go](https://medium.com/@andrewmagic/golang-testing-tantsi-na-tylyazyi-d95851c14a25)

Maintenant que vous avez compris l'importance et la facilité de l'écriture de tests en Go, n'hésitez pas à les utiliser pour garantir la qualité de votre code !