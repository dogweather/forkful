---
title:                "Ruby: Création de nombres aléatoires"
simple_title:         "Création de nombres aléatoires"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

##Pourquoi

Générer des nombres aléatoires est un aspect important de la programmation car cela permet aux développeurs de créer des simulations, des jeux ou tout autre programme nécessitant une composante aléatoire. Cela peut également être utile dans les tests unitaires pour s'assurer que toutes les conditions sont prises en compte.

##Comment faire

Pour générer des nombres aléatoires en Ruby, nous utilisons la méthode ``` rand() ```. Par défaut, cette méthode renvoie un nombre aléatoire entre 0 et 1, par exemple ``` 0.571824523 ```. Pour obtenir un nombre aléatoire dans une plage spécifique, nous pouvons utiliser l'argument ``` rand(max) ``` où ``` max ``` est le nombre maximum que nous voulons inclure. Par exemple, pour obtenir un nombre aléatoire entre 1 et 10, nous utiliserions ``` rand(10) + 1 ```. Il est également possible de spécifier une plage de nombres, par exemple ``` rand(1..100) ``` renverra un nombre aléatoire entre 1 et 100.

Il est également possible de générer des entiers aléatoires en utilisant la méthode ``` rand(max).to_i ```, qui va arrondir le nombre renvoyé par la méthode ``` rand() ``` à un entier.

##Plongée en profondeur

La méthode ``` rand() ``` utilise un générateur de nombres pseudos aléatoires, qui utilise une formule mathématique pour produire une séquence de nombres a priori aléatoires. Cependant, cette méthode peut être prévisible par des hackers ou des cybercriminels, surtout si elle est utilisée pour la génération de mots de passe ou de clés de chiffrement. Dans de tels cas, il est préférable d'utiliser une bibliothèque de génération de nombres aléatoires cryptographiquement sécurisée telle que ``` SecureRandom ```.

##Voir aussi

- [Documentation officielle de Ruby sur la méthode rand()](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-rand)
- [Documentation officielle de la bibliothèque SecureRandom](https://ruby-doc.org/stdlib-2.7.1/libdoc/securerandom/rdoc/SecureRandom.html)
- [Article sur la sécurité de la génération de nombres aléatoires en Ruby](https://www.internet-technologies.eu/random-numbers-in-ruby/)