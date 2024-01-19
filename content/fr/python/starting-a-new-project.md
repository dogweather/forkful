---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Python"
category:             "Python"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Démarrer un nouveau projet en Python : Quoi et Pourquoi ?

## Quoi & pourquoi ?

Démarrer un nouveau projet en Python signifie créer un nouvel espace de travail pour développer et structurer votre code. Les programmeurs font ça pour garder leur code organisé et éviter le mélange du code de différents projets.

## Comment faire :

Initialisez votre projet avec ces codes simples en Python. Voici un exemple de comment vous pouvez créer un nouveau fichier Python et écrire un peu de code dedans :

```Python
# Création d'un nouveau fichier Python
nouveau_fichier = open("MonProjet.py", "w")

# Ecriture de code dans le fichier
nouveau_fichier.write("print('Bonjour le Monde !')")

# Fermeture du fichier
nouveau_fichier.close()
```

Après avoir exécuté ce script, vérifiez dans votre répertoire de travail ; vous verrez un nouveau fichier appelé `MonProjet.py` qui contient une simple commande print.

## Plongée en profondeur

Historiquement, les programmeurs ont toujours eu besoin de garder leur code organisé. Python, avec sa philosophie de simplicité et de lisibilité, a fortement encouragé cette pratique. Il existe des alternatives pour démarrer un nouveau projet, comme la création manuelle de fichiers et de répertoires, mais Python simplifie ce processus avec des commandes intégrées.

En plus de simplement créer un nouveau fichier, Python vous permet également d'ajouter des packages et des modules, d'initialiser un gestionnaire de versions comme Git et de créer un environnement virtuel, si nécessaire. Ces fonctionnalités apportent une grande flexibilité pour structurer votre projet et gérer ses dépendances.

## Voir aussi

1. [Création d'un projet Python dans PyCharm](https://www.jetbrains.com/help/pycharm/creating-and-running-your-first-python-project.html) 
2. [Gérer les projets Python avec Pipenv](https://realpython.com/pipenv-guide/) 
3. [Documentation officielle Python sur les modules](https://docs.python.org/3/tutorial/modules.html)