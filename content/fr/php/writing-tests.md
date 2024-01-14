---
title:    "PHP: Écriture de tests"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire des tests est une pratique essentielle pour tout programmeur PHP. Les tests permettent de s'assurer que notre code fonctionne correctement, et de détecter et corriger les bugs avant qu'ils ne deviennent un problème pour les utilisateurs finaux.

## Comment faire

Pour écrire des tests en PHP, il existe plusieurs frameworks populaires tels que PHPUnit ou Codeception. Voici un exemple de code utilisant PHPUnit pour tester une fonction qui vérifie si un nombre est pair :

```PHP
public function testIsEvenNumber(){
    $number = 4;
    $result = isEven($number);
    
    $this->assertTrue($result);
}
```

Dans cet exemple, nous définissons une fonction de test nommée "testIsEvenNumber" qui utilise la méthode "assertTrue" de PHPUnit pour vérifier si la variable $result est égale à true.

Voici le résultat de l'exécution de ce test :

```
Time: 16 ms, Memory: 6.00 MB

OK (1 test, 1 assertion)
```

Nous pouvons également utiliser des assertions telles que "assertFalse" pour vérifier si un résultat est faux, ou encore "assertEquals" pour tester si deux variables sont égales.

## Plongée en profondeur

Écrire des tests n'est pas seulement utile pour s'assurer du bon fonctionnement de notre code, c'est aussi un moyen efficace d'améliorer sa qualité. En écrivant des tests, nous sommes obligés de réfléchir à différentes possibilités et cas d'utilisation, ce qui peut mener à une meilleure architecture de notre code.

De plus, les tests automatisés nous font gagner du temps à long terme. Une fois que nos tests sont en place, nous pouvons les exécuter à tout moment pour vérifier que notre code n'a pas été cassé par une nouvelle modification.

## Voir aussi

- [PHPUnit](https://phpunit.de/)
- [Codeception](https://codeception.com/)
- [Why We Write Tests](https://www.thoughtworks.com/insights/blog/why-we-write-tests)
- [Les bonnes pratiques pour écrire des tests unitaires en PHP](https://blog.engineering.publicissapient.fr/2019/03/15/les-bonnes-pratiques-pour-ecrire-des-tests-unitaires-en-php/)