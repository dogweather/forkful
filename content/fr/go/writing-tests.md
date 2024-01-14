---
title:                "Go: Écriture de tests"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi écrire des tests en Go ?

Ecrire des tests est une pratique importante dans le développement de logiciels en Go. Cela permet de s'assurer que le code écrit est fonctionnel et qu'il n'y a pas de bugs ou d'erreurs qui pourraient causer des problèmes dans le futur. Les tests permettent également de documenter le code et de faciliter sa maintenance.

## Comment écrire des tests en Go ?

Pour écrire des tests en Go, vous devez d'abord créer un fichier portant le nom de votre code suivi de "_test.go". Par exemple, si vous avez un fichier "calcul.go", votre fichier de test sera nommé "calcul_test.go".

Ensuite, vous devez importer le package "testing" dans votre fichier de test. Ce package fournit des fonctions utiles pour écrire des tests en Go.

Voici un exemple de code pour tester une fonction de calcul de la somme de deux nombres :

```Go
import (
    "testing"
)

func TestSomme(t *testing.T) {
    resultat := calculerSomme(5, 7)
    attendu := 12
    if resultat != attendu {
        t.Errorf("Somme incorrecte, obtenu: %d, attendu: %d", resultat, attendu)
    }
}
```

Ce code utilise la fonction "Test" fournie par le package "testing" pour exécuter un ensemble de tests. La fonction "Errorf" est utilisée pour afficher un message d'erreur en cas de test échoué.

Vous pouvez également utiliser la fonction "Fatal" pour arrêter l'exécution des tests en cas d'erreur critique.

Pour plus d'exemples et d'informations sur l'écriture de tests en Go, vous pouvez consulter la documentation officielle sur le sujet : https://golang.org/pkg/testing/.

## Plongez plus profondément dans l'écriture de tests

L'écriture de tests en Go va au-delà de simples vérifications de résultats. Vous pouvez également tester des cas d'erreurs, des goroutines, des fonctions asynchrones et bien plus encore.

De plus, il existe des outils tels que GoConvey qui facilitent l'écriture et l'exécution de tests en fournissant une interface web pour visualiser les résultats et les couvertures de code. Vous pouvez en savoir plus sur cet outil ici : https://github.com/goconvey.

Il est également important de savoir que les tests en Go peuvent être exécutés à tout moment grâce à la commande "go test". Vous pouvez également spécifier les tests à exécuter en utilisant des expressions régulières pour filtrer les fonctions à tester.

## Voir aussi

- Documentation officielle sur les tests en Go : https://golang.org/pkg/testing/
- Outil GoConvey pour faciliter l'écriture de tests : https://github.com/goconvey