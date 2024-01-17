---
title:                "Téléchargement d'une page web"
html_title:           "Bash: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi ?
Télécharger une page web signifie récupérer les fichiers et le contenu qui composent une page web à partir d'Internet et les enregistrer sur votre ordinateur. Les programmeurs le font souvent pour automatiser le processus de collecte de données ou pour créer des applications qui ont besoin d'accéder à des informations en ligne.

## Comment faire :
Voici un exemple de code Bash pour télécharger une page web en utilisant la commande `wget` :
```
#!/bin/bash
wget https://www.example.com/page.html
```
Lorsque vous exécutez ce script, la page sera téléchargée et enregistrée sur votre ordinateur. Vous pouvez également spécifier un chemin de destination pour enregistrer le fichier en ajoutant l'option `-O` suivi du nom de fichier souhaité, par exemple : `wget https://www.example.com/page.html -O page.html`.

## Exploration en profondeur :
Télécharger des pages web est une pratique courante dans le développement web et la science des données. Il existe également d'autres outils disponibles pour réaliser cette tâche, tels que cURL ou Python avec la bibliothèque Requests. Pour les développeurs, il peut être utile de comprendre les protocoles sous-jacents utilisés pour télécharger des pages web, tels que HTTP et HTTPS.

## Voir aussi :
- [Commande wget](https://www.gnu.org/software/wget/)
- [Site officiel de cURL](https://curl.haxx.se/docs/)
- [Bibliothèque Requests pour Python](https://requests.readthedocs.io/en/master/)