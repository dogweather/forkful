---
title:                "Le téléchargement d'une page web"
html_title:           "Bash: Le téléchargement d'une page web"
simple_title:         "Le téléchargement d'une page web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous voulez télécharger une page web pour l'utiliser hors ligne ou pour accéder à son contenu plus tard sans avoir besoin d'une connexion Internet ? Dans cet article, je vais vous montrer comment le faire en utilisant Bash, le langage de script couramment utilisé sur les systèmes d'exploitation basés sur Unix.

## Comment faire

Tout d'abord, ouvrez votre terminal et déplacez-vous dans le répertoire où vous souhaitez enregistrer la page web téléchargée. Ensuite, utilisez la commande "wget" suivie de l'URL de la page web que vous souhaitez télécharger. Voici un exemple :

```Bash
wget https://www.example.com/
```

Cette commande téléchargera la page d'accueil du site example.com dans le répertoire actuel. Si vous souhaitez enregistrer la page sous un nom de fichier spécifique, utilisez l'option "-O" suivie du nom de fichier de votre choix :

```Bash
wget https://www.example.com/ -O example.html
```

Vous pouvez également utiliser "curl" pour télécharger une page web. La syntaxe est similaire à celle de "wget" :

```Bash
curl -O https://www.example.com/ -o example.html
```

La différence est que "curl" téléchargera uniquement la page web spécifiée sans suivre les liens sur cette page.

Une fois que le téléchargement est terminé, vous pouvez ouvrir le fichier dans votre navigateur ou utiliser d'autres outils pour manipuler le contenu téléchargé.

## Deep Dive

Les commandes "wget" et "curl" utilisent toutes deux le protocole HTTP pour télécharger des fichiers. Cela signifie que si une page web utilise des formulaires ou un autre type de contenu interactif, vous ne pourrez pas interagir avec ce contenu de la même manière que vous le feriez en ligne.

De plus, si la page web nécessite une authentification, vous devrez spécifier vos identifiants dans la commande pour que le téléchargement puisse être effectué. Consultez la documentation de "wget" et "curl" pour plus d'informations sur l'utilisation des options pour gérer cela.

## Voir aussi

- [Documentation "wget"](https://www.gnu.org/software/wget/)
- [Documentation "curl"](https://curl.haxx.se/docs/)
- [Guide de survie en ligne de commande pour Bash (en anglais)](https://devhints.io/bash)