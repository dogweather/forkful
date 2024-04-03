---
date: 2024-01-26 01:08:14.614754-07:00
description: "Le logging est le processus d'enregistrement des \xE9v\xE9nements d'une\
  \ application pendant l'ex\xE9cution d'un programme, fournissant un fil d'Ariane\
  \ pour\u2026"
lastmod: '2024-03-13T22:44:57.243402-06:00'
model: gpt-4-1106-preview
summary: "Le logging est le processus d'enregistrement des \xE9v\xE9nements d'une\
  \ application pendant l'ex\xE9cution d'un programme, fournissant un fil d'Ariane\
  \ pour l'analyse post-mortem et la surveillance en temps r\xE9el."
title: Journalisation
weight: 17
---

## Comment faire :
Python est fourni avec un module intégré pour le logging. Voici une configuration de base :
```Python
import logging

# Configuration de base du logging
logging.basicConfig(level=logging.INFO)

# Messages de logging
logging.debug('Ceci est un message de débogage')
logging.info('Information sur ce que votre programme vient de faire')
logging.warning('Un message d'avertissement')
logging.error('Une erreur est survenue')
logging.critical('Le programme ne peut pas se récupérer !')
```
Lorsque vous exécutez ce code, vous verrez la sortie suivante (comme le niveau par défaut est WARNING, les messages de debug et d'info ne seront pas affichés) :
```
WARNING:root:Un message d'avertissement
ERROR:root:Une erreur est survenue
CRITICAL:root:Le programme ne peut pas se récupérer !
```
Vous pouvez également configurer le logging pour écrire dans un fichier au lieu de la console :
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Maintenant vos logs seront dirigés vers le fichier 'app.log'.

## Plongée en profondeur
Le logging existe depuis les premiers jours de la programmation, avec les journaux système étant une des plus anciennes formes de stockage persistant en dehors des fichiers réels contenant des données. Histoire mise à part, le concept principal du logging reste essentiellement inchangé, bien que les outils aient évolué.

Le module `logging` de Python est assez puissant et flexible. Il permet aux programmeurs de définir différents niveaux de logs (DEBUG, INFO, WARNING, ERROR, CRITICAL) qui peuvent aider à catégoriser et filtrer les logs. Il a un système de loggers hiérarchique, ce qui signifie que vous pouvez avoir des relations parent-enfant entre les loggers et propager les messages dans la chaîne.

Parmi les alternatives, on trouve des bibliothèques tierces comme Loguru ou structlog qui offrent des fonctionnalités améliorées et une interface plus simple que le module de logging intégré. Elles peuvent fournir une sortie plus jolie, une meilleure sérialisation des données structurées, et des manières plus intuitives de gérer la configuration des logs.

Concernant la mise en œuvre, lors de la configuration du logging, il est important de le faire une seule fois au démarrage de votre application. Il est recommandé de le configurer au niveau du module en utilisant `logging.getLogger(__name__)` pour suivre les meilleures pratiques de logging de Python.

Le logging ne devrait pas affecter drastiquement les performances d'une application dans des circonstances normales. Cependant, il faut faire attention à ce qui est loggé : un logging trop verbeux, en particulier aux niveaux DEBUG, peut ralentir une application et remplir rapidement l'espace de stockage des fichiers de logs.

## Voir Aussi
Pour en savoir plus sur le module de logging de Python, consultez le manuel officiel de Python sur le logging pour de bons exemples et des pratiques recommandées : https://docs.python.org/3/howto/logging-cookbook.html

Pour un examen approfondi du logging structuré et comment il peut aider à rendre les logs plus informatifs et plus faciles à analyser, Loguru est bien documenté : https://loguru.readthedocs.io

En outre, considérez l'examen de la méthodologie de l'application 12-facteurs, en particulier la section sur les logs pour une vision moderne du logging d'application : https://12factor.net/logs
