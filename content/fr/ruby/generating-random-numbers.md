---
title:                "Génération de nombres aléatoires"
html_title:           "Ruby: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Tu te demandes peut-être pourquoi tu voudrais générer des nombres aléatoires en utilisant Ruby. Eh bien, la génération de nombres aléatoires peut être utile dans de nombreux cas, comme la génération de données de test pour ton code ou la simulation de résultats aléatoires pour un jeu.

## Comment faire

Générer des nombres aléatoires en Ruby est assez simple. Tout d'abord, tu devras utiliser la méthode ```rand()``` en spécifiant la plage de nombres à l'intérieur des parenthèses. Par exemple, si tu veux générer un nombre aléatoire entre 1 et 10, tu peux utiliser ```rand(1..10)```.

Voici un exemple de code qui génère 5 nombres aléatoires entre 1 et 100 et les imprime à l'écran :

```Ruby
(1..5).each do
    puts rand(1..100)
end
```

Voici le résultat de cet exemple :

```
52
16
85
41
99
```

## Plongée profonde

La méthode ```rand()``` utilise en fait une formule mathématique pour générer des nombres pseudo-aléatoires. Cela signifie que les nombres ne sont pas vraiment aléatoires, mais ils semblent l'être. Cela peut sembler étrange, mais en informatique, il est difficile de générer des nombres vraiment aléatoires, donc les méthodes pseudo-aléatoires sont souvent utilisées.

Il est également important de noter que la méthode ```rand()``` utilise comme base un nombre appelé "seed". Ce nombre peut être spécifié en utilisant ```srand()```, ce qui te donnera toujours la même séquence de nombres aléatoires si tu utilises le même seed.

## Voir aussi
Pour en savoir plus sur la génération de nombres aléatoires en Ruby, tu peux consulter les liens suivants :
- [Documentation officielle de Ruby sur ```rand()```](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-rand)
- [Un tutoriel sur la génération de nombres aléatoires en Ruby](https://www.educative.io/edpresso/ruby-generating-random-numbers)

Et n'oublie pas de t'amuser en expérimentant avec la génération de nombres aléatoires en utilisant Ruby !