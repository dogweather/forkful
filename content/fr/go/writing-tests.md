---
title:                "Ecrire des tests"
html_title:           "Go: Ecrire des tests"
simple_title:         "Ecrire des tests"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-tests.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est ?
L'écriture de tests est un processus par lequel les programmeurs vérifient si leur code fonctionne correctement. Cela implique de créer des cas de test pour couvrir différents scénarios et d'écrire du code pour s'assurer que le code initial fonctionne comme prévu.

Pourquoi le faire ?
Les tests sont importants car ils permettent de s'assurer que le code est exempt d'erreurs et fonctionne comme prévu. Cela peut aider à réduire les bogues et à améliorer la qualité du code. De plus, en écrivant des tests, les programmeurs peuvent également détecter les problèmes plus tôt dans le processus de développement et les corriger avant qu'ils ne deviennent des problèmes plus importants.

Comment faire ?
Voici un exemple de code en Go pour illustrer comment écrire des tests :

```
package main

import "fmt"

// Fonction d'addition simple
func addition(a, b int) int {
	return a + b
}

func main() {
	// Tests pour la fonction addition
	tests := []struct {
		input1 int
		input2 int
		expectedOutput int
	}{
		{2, 2, 4},
		{5, 7, 12},
		{-3, 10, 7},
	}

	for _, test := range tests {
		result := addition(test.input1, test.input2)
		if result == test.expectedOutput {
			fmt.Printf("Le résultat de %d + %d est %d. Test réussi !\n", test.input1, test.input2, result)
		} else {
			fmt.Printf("Le résultat de %d + %d était incorrect. Le résultat réel est %d\n", test.input1, test.input2, result)
		}
	}
}
```

Résultat du code :

```
Le résultat de 2 + 2 est 4. Test réussi !
Le résultat de 5 + 7 est 12. Test réussi !
Le résultat de -3 + 10 est 7. Test réussi !
```

Plongée en profondeur
Les tests unitaires, qui sont des tests pour des parties spécifiques du code, ont été introduits pour la première fois dans le cadre de la méthodologie de développement piloté par les tests (TDD). Cette approche consiste à écrire les tests avant même l'écriture du code, et à s'assurer que tous les tests réussissent avant de passer à la phase de développement suivante. Certains développeurs préfèrent également utiliser des outils de test tels que GoConvey pour optimiser le processus de test.

Voir également
- [Apprenez à écrire des tests en Go](https://blog.golang.org/test)
- [Code de l'exemple de test en Go](https://gist.github.com/asankov/5322deccd565ffce0659bd9f16607e5c)
- [Documentation Go sur les tests](https://golang.org/pkg/testing/)