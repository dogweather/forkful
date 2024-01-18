---
title:                "Génération de nombres aléatoires"
html_title:           "Lua: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?

La génération de nombres aléatoires est une technique utilisée par les programmeurs pour créer des valeurs aléatoires dans un programme. Cela permet de simuler des situations aléatoires, d'améliorer la sécurité des mots de passe, et bien plus encore.

# Comment faire:

Voici un exemple simple de code en Lua pour générer un nombre aléatoire entre 1 et 10:

```
math.randomseed(os.time()) -- initialiser la graine pour la génération aléatoire
print(math.random(1,10)) -- afficher un nombre aléatoire entre 1 et 10
```

Output: 5

Vous pouvez également utiliser ```math.random()``` pour des valeurs aléatoires entre 0 et 1:

```
math.randomseed(os.time())
print(math.random()) -- afficher un nombre aléatoire entre 0 et 1
```

Output: 0.6550

Si vous souhaitez générer plusieurs nombres aléatoires en une seule fois, vous pouvez utiliser une boucle:

```
math.randomseed(os.time())
for i=1,5 do -- générer 5 nombres aléatoires
    print(math.random(1,10))
end
```

Output: 7 2 9 4 8

# Plongée en profondeur:

La génération de nombres aléatoires est utilisée depuis longtemps dans le domaine des jeux de hasard, mais elle est également utile pour de nombreux autres cas d'utilisation. Les autres langages de programmation ont également leurs propres fonctions pour générer des nombres aléatoires, tels que ```random()``` en Python ou ```rand()`` en C. Il est important de noter que les nombres générés par les ordinateurs ne sont pas vraiment aléatoires, mais ils sont calculés à partir d'une "graine" initiale et d'un algorithme de génération.

# Voir aussi:

Pour en savoir plus sur la génération de nombres aléatoires en Lua, vous pouvez consulter la documentation officielle sur la fonction ```math.random()``` ainsi que d'autres ressources en ligne telles que des tutoriels et des forums communautaires.