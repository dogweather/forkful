---
title:    "Ruby: Génération de nombres aléatoires"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi

Générer des nombres aléatoires peut sembler être une tâche ennuyeuse et inutile, mais c'est en fait une compétence importante pour les programmeurs de Ruby. L'utilisation de nombres aléatoires peut ajouter une dimension d'imprévisibilité à vos programmes et les rendre plus intéressants.

# Comment Faire

La génération de nombres aléatoires en Ruby est assez simple grâce à la méthode `rand()`. Cette méthode prend un argument entier en tant que limite supérieure et renvoie un nombre aléatoire compris entre 0 et cette limite. Voyons un exemple :

````Ruby
puts rand(10) # renvoie un nombre aléatoire entre 0 et 10
````

Vous pouvez également spécifier une limite inférieure en utilisant une virgule :

````Ruby
puts rand(1..5) # renvoie un nombre aléatoire entre 1 et 5
````

Si vous souhaitez générer un nombre aléatoire à virgule flottante, vous pouvez utiliser la méthode `randf()` :

````Ruby
puts randf() # renvoie un nombre aléatoire entre 0 et 1
````

Et si vous souhaitez obtenir un nombre aléatoire spécifique à chaque utilisation de votre programme, vous pouvez utiliser la méthode `srand()` pour initialiser la séquence de nombres aléatoires :

````Ruby
srand(12345) # initialise une séquence de nombres aléatoires basée sur le nombre 12345
````

# Plongée Profonde

Si vous voulez vraiment vous plonger dans la génération de nombres aléatoires en Ruby, vous pouvez utiliser la classe `Random` pour avoir un contrôle plus précis sur la génération de nombres. Par exemple, si vous voulez générer un nombre aléatoire entre deux limites sans inclure ces limites, vous pouvez utiliser la méthode `randint()` :

````Ruby
rng = Random.new
puts rng.randint(1...10) # renvoie un nombre aléatoire entre 1 et 10, excluant 1 et 10
````

De plus, la classe `Random` offre également des méthodes pour générer des chaînes aléatoires, des nombres binaires et même des objets aléatoires à partir d'un tableau.

# Voir Aussi

- [Documentation officielle de Ruby sur la génération de nombres aléatoires](https://ruby-doc.org/core-2.7.1/Random.html)
- [Article sur la génération de nombres aléatoires en Ruby](https://www.honeybadger.io/blog/generating-random-numbers-for-fun-and-profit-in-ruby/)