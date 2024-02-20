---
date: 2024-01-20 18:04:16.830388-07:00
description: "Commencer un nouveau projet, c'est mettre les bases d'une application\
  \ ou d'un script. On le fait pour r\xE9soudre un probl\xE8me, tester une id\xE9\
  e, ou apprendre\u2026"
lastmod: 2024-02-19 22:05:16.138928
model: gpt-4-1106-preview
summary: "Commencer un nouveau projet, c'est mettre les bases d'une application ou\
  \ d'un script. On le fait pour r\xE9soudre un probl\xE8me, tester une id\xE9e, ou\
  \ apprendre\u2026"
title: Lancement d'un nouveau projet
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Commencer un nouveau projet, c'est mettre les bases d'une application ou d'un script. On le fait pour résoudre un problème, tester une idée, ou apprendre quelque chose de nouveau.

## How to: (Comment faire :)
Pour démarrer, on configure l'environnement et on crée un fichier `main.py` :

```Python
# Installation de l'environnement virtuel
python -m venv mon_projet_env
# Activation de l'environnement virtuel
# Sur Windows:
mon_projet_env\Scripts\activate.bat
# Sur Unix ou MacOS:
source mon_projet_env/bin/activate
# Création du fichier main.py
echo "print('Salut, nouveau projet!')" > main.py
# Exécution du fichier
python main.py
```

Résultat :

```Python
Salut, nouveau projet!
```

## Deep Dive (Plongée en profondeur)
Historiquement, les programmeurs créaient des scripts sans isoler les dépendances, ce qui pouvait mener à des conflits entre projets. L'utilisation d'environnements virtuels (`venv`) règle ce problème. Des alternatives existent comme `pipenv` ou `conda`, qui offrent des fonctionnalités supplémentaires. Au niveau de l'implémentation, bien structurer son projet dès le départ est crucial : penser à bien nommer les dossiers et fichiers, organiser le code en modules et paquets, et documenter son code. Cela simplifie la maintenance et la collaboration.

## See Also (Voir aussi)
- Documentation officielle pour venv : https://docs.python.org/3/library/venv.html
- Guide sur les environnements virtuels : https://realpython.com/python-virtual-environments-a-primer/
- Bonnes pratiques pour structurer un projet : https://docs.python-guide.org/writing/structure/
