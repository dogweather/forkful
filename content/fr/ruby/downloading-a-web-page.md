---
title:                "Téléchargement d'une page web"
html_title:           "Ruby: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes intéressé par le développement web, alors vous avez probablement entendu parler du langage de programmation Ruby. Ruby est un langage élégant et flexible, utilisé pour créer des sites web dynamiques et des applications. Dans cet article, nous allons explorer comment utiliser Ruby pour télécharger une page web et manipuler son contenu.

## Comment faire

La première étape pour télécharger une page web est d'installer le package Ruby `open-uri`. Ouvrez votre terminal et tapez :

```Ruby
gem install open-uri
```

Maintenant que nous avons le package installé, nous pouvons commencer à télécharger une page web en utilisant la méthode `open()` de `open-uri` :

```Ruby
require 'open-uri'

html = open('https://www.example.com').read
puts html
```

Dans cet exemple, nous utilisons la méthode `read` pour lire le contenu de la page téléchargée et le stocker dans une variable `html`. Ensuite, nous imprimons le contenu à l'écran avec `puts`. Vous pouvez également enregistrer le contenu dans un fichier en utilisant la méthode `write` :

```Ruby
open('https://www.example.com').write(html)
```

Maintenant que nous avons téléchargé notre page web, nous pouvons la manipuler en utilisant des méthodes de traitement de strings de Ruby telles que `gsub` (remplacer une chaîne de caractères par une autre) ou `scan` (extraire des informations à partir d'une chaîne de caractères). Par exemple, si nous voulons extraire tous les liens de la page et les stocker dans un tableau, nous pouvons utiliser :

```Ruby
links = html.scan(/<a.+?href=\"(.+?)\".+?>/)
```

Nous pouvons ensuite parcourir ce tableau et imprimer chaque lien à l'écran ou les enregistrer dans un fichier. Avec un peu de créativité, les possibilités de manipulation sont infinies !

## Plongée en profondeur

Télécharger une page web peut sembler simple, mais il y a en fait plusieurs choses à prendre en compte. Par exemple, des erreurs peuvent se produire pendant le téléchargement ou le contenu de la page peut être encodé d'une manière qui n'est pas compatible avec Ruby. Dans ce cas, nous pouvons utiliser `open-uri` pour gérer ces problèmes en utilisant des options supplémentaires telles que `read_timeout` (fixer une limite de temps pour le téléchargement) ou `charset` (spécifier l'encodage du contenu).

```Ruby
html = open('https://www.example.com', read_timeout: 10, charset: 'utf-8').read
```

En outre, vous pouvez également utiliser la méthode `URI::HTTPS.build` pour construire une URL personnelle avec des paramètres comme dans l'exemple suivant :

```Ruby
url = URI::HTTPS.build(host: 'www.example.com', path: '/page', query: 'id=1')
html = open(url).read
```

En utilisant ces options et méthodes avancées, vous pouvez personnaliser le téléchargement de votre page web en fonction de vos besoins spécifiques.

## Voir aussi

Maintenant que vous avez appris à télécharger une page web en utilisant Ruby, vous pouvez explorer les autres possibilités du langage pour créer des sites web dynamiques et des applications. Voici quelques ressources qui pourraient vous intéresser :

- [Documentation officielle de Ruby (EN)](https://www.ruby-lang.org/en/documentation/)
- [Ruby on Rails - le framework web Ruby le plus populaire](https://rubyonrails.org/)
- [The Ruby Toolbox - répertoire de bibliothèques et outils Ruby](https://www.ruby-toolbox.com/)

À vous de jouer !