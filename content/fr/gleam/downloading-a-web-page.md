---
title:                "Téléchargement d'une page web"
html_title:           "Gleam: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur web ou un programmeur à la recherche d'un moyen simple et efficace de télécharger des pages web, alors vous êtes au bon endroit ! Dans cet article, nous allons vous montrer comment utiliser Gleam pour télécharger rapidement et facilement une page web.

## Comment faire

Télécharger une page web avec Gleam est très simple. Tout d'abord, vous devez importer la bibliothèque http de Gleam en utilisant la commande suivante :

```Gleam
import http
```

Ensuite, vous pouvez utiliser la fonction `http.get` pour télécharger une page web en fournissant simplement l'URL de la page en tant que paramètre. Voici un exemple pour télécharger la page d'accueil de Google :

```Gleam
response := http.get("https://www.google.com/")
```

La variable `response` contiendra maintenant le contenu de la page téléchargée. Vous pouvez utiliser la fonction `http.body_text` pour obtenir uniquement le texte du corps de la page.

```Gleam
body := http.body_text(response)
```

La variable `body` contiendra maintenant le texte de la page téléchargée. Vous pouvez également spécifier des en-têtes HTTP supplémentaires dans un dictionnaire en tant que deuxième paramètre de la fonction `http.get`.

## Deep Dive

Maintenant que vous savez comment télécharger une page web avec Gleam, voici quelques informations plus détaillées sur la fonction `http.get`. Elle renvoie un type `Result`, ce qui signifie qu'elle peut retourner soit une `Ok` avec le contenu de la page, soit une `Error` avec un message d'erreur en cas d'échec du téléchargement.

De plus, vous pouvez également spécifier une fonction de rappel en tant que troisième paramètre de la fonction `http.get` pour effectuer des opérations supplémentaires sur les données téléchargées, telles que la mise en forme ou l'enregistrement dans un fichier. Cependant, cette fonction de rappel ne sera exécutée que si le téléchargement est un succès.

## Voir aussi

Maintenant que vous savez comment télécharger une page web avec Gleam, vous pouvez l'utiliser dans vos projets pour récupérer des données à partir de pages web. Pour en savoir plus sur les autres fonctionnalités de Gleam, consultez la documentation officielle et découvrez comment vous pouvez l'intégrer à votre flux de travail de développement.

- Documentation de Gleam : https://gleam.run/
- GitHub : https://github.com/gleam-lang/gleam