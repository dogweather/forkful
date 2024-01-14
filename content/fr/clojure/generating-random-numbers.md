---
title:                "Clojure: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## PourquoiLa génération de nombres aléatoires est un concept important dans la programmation car elle permet de créer des éléments aléatoires dans un programme, ce qui le rend plus dynamique et imprévisible. Cela peut être utile pour simuler des situations réalistes ou pour créer des éléments de jeu tels que des cartes ou des dés.  ## CommentFonction ```(rand)```: Cette fonction renvoie un nombre aléatoire compris entre 0 (inclus) et 1 (exclu).Exemple: ```Clojure (rand)``` Sortie: 0,3247102Fonction ```(rand-int n)```: Cette fonction renvoie un nombre entier aléatoire compris entre 0 (inclus) et n (exclu).Exemple: ```Clojure (rand-int 10)``` Sortie: 5  ## Plongée profondeLa génération de nombres aléatoires peut sembler simple, mais elle est en fait basée sur des algorithmes sophistiqués qui utilisent des nombres pseudo-aléatoires pour créer une séquence de nombres apparemment aléatoires. Il est important de comprendre ces concepts pour utiliser de manière efficace et fiable la génération de nombres aléatoires dans vos programmes.  ## Voir aussi
- Documentation officielle de Clojure sur la génération de nombres aléatoires: https://clojure.org/reference/java_interop#_random_numbers
- Article sur la différence entre les nombres pseudo-aléatoires et les nombres vraiment aléatoires: https://fr.wikipedia.org/wiki/Nombre_pseudo-al%C3%A9atoire
- E-book gratuit sur les concepts de la génération de nombres aléatoires: https://www.datagenius.com/randomnumbergeneration.html