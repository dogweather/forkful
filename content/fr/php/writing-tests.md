---
title:                "PHP: Ecriture de tests"
simple_title:         "Ecriture de tests"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi écrire des tests en PHP?

Écrire des tests est une pratique essentielle en programmation PHP. Cela permet de vérifier la robustesse de votre code, de le maintenir à jour et de détecter rapidement les erreurs.

## Comment procéder?

Voici un exemple de code avec des tests pour une fonction de calcul de moyenne:

```PHP
<?php
function calculer_moyenne($notes){
   $total = 0;
   $nb_notes = count($notes);

   for($i = 0; $i < $nb_notes; $i++){
      $total += $notes[$i];
   }

   $moyenne = $total / $nb_notes;
   return $moyenne;
}

// Test unitaire pour vérifier si la fonction renvoie la bonne moyenne
$notes = [10, 15, 18];
$moyenne = calculer_moyenne($notes);

if($moyenne == 14.33){
   echo "Le test a réussi!";
} else {
   echo "Le test a échoué, vérifiez votre code.";
}
```

Résultat attendu: "Le test a réussi!"

## Plongée en profondeur

L'écriture de tests vous permet également de suivre l'évolution de votre code. En ajoutant des tests régulièrement, vous vous assurez que les nouvelles fonctionnalités ou modifications n'ont pas d'impact sur les fonctionnalités existantes.

De plus, les tests facilitent la collaboration en équipe car chacun peut comprendre rapidement le fonctionnement du code et en cas de bug, il est plus facile de le localiser grâce aux tests.

Il existe plusieurs types de tests en PHP, tels que les tests unitaires, les tests d'intégration et les tests fonctionnels. Chacun a son utilité et il est important de les mettre en place dès le début du développement.

## Voir aussi

- [Introduction aux tests en PHP](https://www.php.net/manual/en/function.assert.php)
- [Test Driven Development en pratique avec PHPUnit](https://www.php.net/manual/fr/book.phpunit.php)
- [5 bonnes raisons d'écrire des tests en PHP](https://blog.engineyard.com/5-reasons-to-write-tests-in-php)