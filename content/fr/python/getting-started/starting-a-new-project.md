---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:29:57.341989-07:00
description: 'Comment faire : #.'
lastmod: '2024-03-13T22:44:57.236761-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "D\xE9marrer un nouveau projet"
weight: 1
---

## Comment faire :


### Créer un Environnement Virtuel
Un environnement virtuel est un répertoire autonome qui contient tous les exécutables nécessaires pour utiliser les paquets dont un projet Python aurait besoin. Il est conseillé de créer un environnement virtuel pour chaque projet afin d'éviter les conflits entre les dépendances des projets. Utilisez le module `venv`, qui fait partie de la bibliothèque standard de Python.

```shell
# Remplacez 'myproject' par le nom de votre projet
python3 -m venv myproject-env
```

Pour activer l'environnement virtuel :

Sous Windows :
```shell
myproject-env\Scripts\activate.bat
```

Sous Unix ou MacOS :
```shell
source myproject-env/bin/activate
```

Exemple de Sortie (la sortie peut légèrement varier selon l'OS) :
```shell
(myproject-env) $
```

### Installer des Paquets
Utilisez `pip`, l'installateur de paquets pour Python, pour installer, mettre à niveau et supprimer des paquets. Voici comment vous pouvez installer une bibliothèque tierce populaire, `requests`, pour faire des requêtes HTTP :

```shell
pip install requests
```

Exemple de Sortie :
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### Mettre en Place une Structure de Projet
Un projet Python typique pourrait ressembler à cela :

```
myproject/
│
├── myproject-env/    # Environnement virtuel
├── docs/             # Documentation
├── tests/            # Tests unitaires et d'intégration
│   └── __init__.py
├── myproject/        # Code source du projet
│   ├── __init__.py
│   └── main.py
├── setup.py          # Fichier de configuration du projet
└── README.md         # Vue d'ensemble du projet
```

### Créez Votre Premier Programme
Créez un fichier `main.py` dans le répertoire `myproject`. Voici un exemple de programme simple :

```python
# myproject/myproject/main.py
def greet(name):
    return f"Bonjour, {name}!"

if __name__ == "__main__":
    print(greet("le monde"))
```

Exécutez votre programme :

```shell
python myproject/main.py
```

Exemple de Sortie :
```shell
Bonjour, le monde!
```

### Utiliser un Cadre de Travail pour les Grands Projets
Pour les grands projets, en particulier les applications web, des cadres de travail comme Django ou Flask sont inestimables. Voici comment installer Flask et créer une simple application web "Hello, World" :

```shell
pip install Flask
```

Créez un fichier `app.py` avec le contenu suivant :

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Bonjour, le monde !</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Exécutez l'application Flask :

```shell
flask run
```

Exemple de Sortie :
```shell
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

Naviguez vers `http://127.0.0.1:5000/` dans votre navigateur web, et vous devriez voir le message "Bonjour, le monde !".
