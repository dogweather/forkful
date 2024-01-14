---
title:    "C++: Écrire des tests"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-tests.md"
---

{{< edit_this_page >}}

### Pourquoi

Les tests sont une partie essentielle de tout processus de développement logiciel. Ils permettent de vérifier la qualité du code et de détecter les erreurs avant qu'elles ne causent des problèmes aux utilisateurs finaux. De plus, écrire des tests garantit que le code reste fonctionnel au fil du temps, même après des mises à jour ou des modifications.

### Comment faire

Pour écrire des tests efficaces en C++, vous pouvez utiliser le framework de test unitaire Catch2. Tout d'abord, installez Catch2 sur votre système et incluez le dossier `catch2` dans votre projet. Ensuite, créez un nouveau fichier de test avec l'extension `.cpp` et incluez `catch.hpp` au début du fichier.

```C++
#include "catch.hpp"

// Écrivez vos tests ici
```

Ensuite, vous pouvez définir vos tests en utilisant la macro `TEST_CASE` de Catch2. Par exemple, pour tester une fonction qui ajoute deux nombres, vous pouvez écrire :

```C++
TEST_CASE( "Additionner deux nombres" ) {
    REQUIRE( addition(2, 3) == 5 );
}
```

Vous pouvez également utiliser la macro `REQUIRE` pour spécifier les conditions que votre code doit satisfaire et ainsi vérifier si elles sont remplies dans vos tests. Si une condition n'est pas remplie, le test échouera et vous pourrez corriger votre code en conséquence.

Pour exécuter vos tests, vous pouvez simplement compiler et exécuter votre fichier de test. Si tout se passe bien, vous devriez voir un message de réussite pour chaque test.

### Plongée en profondeur

Écrire des tests peut sembler fastidieux et chronophage, mais cela peut vous faire économiser beaucoup de temps et d'efforts à long terme. En plus de vérifier la qualité de votre code, les tests peuvent également servir de documentation pour vos fonctions et vos classes. De plus, en utilisant Catch2, vous pouvez facilement intégrer des tests dans votre processus de développement en les exécutant à chaque fois que vous apportez des modifications à votre code.

De plus, la pratique du test-driven development, où les tests sont écrits avant même le code de production, peut vous aider à concevoir un code de meilleure qualité dès le début.

### Voir aussi

- [Catch2 sur GitHub](https://github.com/catchorg/Catch2)
- [Guide de démarrage Catch2](https://github.com/catchorg/Catch2/blob/master/docs/tutorial.md)
- [Test-Driven Development (TDD): le guide complet](https://medium.com/swlh/test-driven-development-tdd-le-guide-complet-1-dans-ce-tutoriel-nous-plongeons-dans-la-6bd4febf3b8a)