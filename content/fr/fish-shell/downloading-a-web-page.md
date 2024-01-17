---
title:                "Téléchargement d'une page web"
html_title:           "Fish Shell: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Télécharger une page web, c'est récupérer le contenu d'un site internet pour pouvoir l'utiliser dans un programme. Les programmeurs font cela pour accéder à des informations spécifiques ou pour automatiser des tâches.

## Comment faire:
Les exemples de code ci-dessous utilisent le shell Fish pour télécharger une page web en utilisant la commande `curl`.

```Fish Shell
curl <url de la page>  # télécharge la page et imprime le contenu dans la console
curl <url de la page> > fichier.html  # télécharge la page et enregistre le contenu dans un fichier HTML
```

Exemple de sortie:

```Fish Shell
<!DOCTYPE html>
<html>
<head>
    <title>Titre de la page</title>
</head>
<body>
    <h1>Bienvenue sur mon site</h1>
    <p>Ce site est dédié à tout ce qui concerne les poissons.</p>
</body>
</html>
```

Vous pouvez également utiliser la commande `wget` pour télécharger des fichiers en ligne à partir du shell Fish.

## Plongée en profondeur:
Avant l'avènement du World Wide Web, les programmeurs utilisaient principalement la fonction `ftp` pour télécharger des fichiers. Avec l'avènement des pages web avec du contenu dynamique, l'utilisation de la commande `curl` est devenue plus courante.

Il existe d'autres outils et bibliothèques pour télécharger des pages web, tels que `Beautiful Soup` en Python ou `Selenium` pour automatiser des interactions avec des pages web. Cependant, l'utilisation de `curl` ou `wget` dans le shell Fish est une option simple et rapide pour des besoins de téléchargement de base.

Lors de l'utilisation de la commande `curl`, il est possible de spécifier des options supplémentaires telles que l'authentification, l'utilisation de proxies et le téléchargement de contenu spécifique. Vous pouvez utiliser la commande `curl --help` pour voir toutes les options disponibles.

## Voir aussi:
- [La documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Le site officiel de curl](https://curl.se/)