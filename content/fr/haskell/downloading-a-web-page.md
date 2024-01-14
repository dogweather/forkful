---
title:                "Haskell: Downloading une page web."
simple_title:         "Downloading une page web."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Télécharger une page Web est une tâche commune pour les programmeurs souhaitant extraire des données ou créer des bots. En utilisant Haskell, vous pouvez facilement automatiser cette tâche en utilisant des outils puissants tels que le paquetage `http-conduit`.

## Comment faire

Pour commencer, vous devrez importer le paquetage `http-conduit` en ajoutant la ligne `import Network.HTTP.Conduit` à votre fichier Haskell. Ensuite, vous pouvez utiliser la fonction `simpleHttp` pour télécharger une page Web. Par exemple, pour télécharger la page d'accueil de Google :

```Haskell
simpleHttp "http://www.google.com"
```

Cela renverra une valeur de type `ByteString` représentant le contenu de la page Web. Vous pouvez ensuite traiter cette valeur comme vous le souhaitez, que ce soit pour extraire des informations spécifiques ou pour enregistrer la page sur votre disque dur.

## Plongée profonde

Le paquetage `http-conduit` offre de nombreuses fonctionnalités avancées telles que la gestion des cookies, la compression, l'authentification et bien plus encore. Vous pouvez également contrôler les paramètres de la requête HTTP en utilisant la fonction `parseUrl` pour créer un objet `Request` et en le passant à `httpLbs` pour exécuter la requête. De plus, vous pouvez gérer les erreurs en utilisant la fonction `catch` pour une gestion plus robuste des erreurs.

## Voir aussi

- Documentation officielle du paquetage `http-conduit` : https://www.stackage.org/package/http-conduit
- Tutoriel sur la récupération de données en Haskell : https://haskell-lang.org/tutorial/data-retrieval
- Exemple de code pour télécharger une page Web en Haskell : https://www.stackbuilders.com/tutorials/haskell/web-scraping/