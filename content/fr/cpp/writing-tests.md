---
title:                "C++: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi écrirer des tests en C++

Il est souvent considéré comme une bonne pratique de tester son code lors de la programmation, surtout en C++, où les erreurs peuvent être plus difficiles à repérer. Écrire des tests peut aider à détecter et à corriger les erreurs avant qu'elles ne causent des problèmes dans votre programme.

## Comment écrire des tests en C++

Pour écrire des tests en C++, vous pouvez utiliser la bibliothèque de tests unitaires Google Test. Voici un exemple de test simple qui vérifie si une fonction retourne le bon résultat :

```C++
#include <gtest/gtest.h>

int square(int x) {
    return x * x;
}

TEST(SquareTest, PositiveNumbers) {
    EXPECT_EQ(square(5), 25);
    EXPECT_EQ(square(10), 100);
}

TEST(SquareTest, NegativeNumbers) {
    EXPECT_EQ(square(-5), 25);
    EXPECT_EQ(square(-10), 100);
}

int main(int argc, char** argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Cette bibliothèque vous permet de définir des cas de tests avec différentes valeurs en entrée et de vérifier si les résultats sont corrects avec la fonction `ASSERT_EQ` de Google Test. Si tous les tests passent, le programme affichera un message de réussite. Sinon, il indiquera quel test a échoué et quel était l'erreur.

## Plongée plus profonde

Écrire des tests peut sembler fastidieux et prendre du temps, mais cela peut vous faire gagner du temps à long terme en réduisant le temps passé à déboguer en cas d'erreur. Cela peut également rendre votre code plus robuste et faciliter les modifications et les mises à jour ultérieures.

Certaines bonnes pratiques à suivre lors de l'écriture de tests en C++ sont les suivantes :

- Testez toutes les fonctions importantes de votre programme.
- Utilisez des valeurs en entrée variées pour vos tests.
- Commentez vos tests pour expliquer pourquoi ils sont importants et ce qu'ils devraient vérifier.

# Voir aussi

- [Documentation de Google Test pour C++](https://github.com/google/googletest/blob/master/googletest/docs/primer.md)
- [Tutoriel sur les tests unitaires en C++](https://www.testedemo.com/tutorials/tutorials.html)