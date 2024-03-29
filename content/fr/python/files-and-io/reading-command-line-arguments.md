---
date: 2024-01-20 17:56:44.477376-07:00
description: "Lire les arguments de ligne de commande permet \xE0 vos scripts Python\
  \ de prendre des donn\xE9es directement depuis le terminal. Les programmeurs utilisent\
  \ cela\u2026"
lastmod: '2024-03-13T22:44:57.252753-06:00'
model: gpt-4-1106-preview
summary: "Lire les arguments de ligne de commande permet \xE0 vos scripts Python de\
  \ prendre des donn\xE9es directement depuis le terminal. Les programmeurs utilisent\
  \ cela\u2026"
title: Lecture des arguments de ligne de commande
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Lire les arguments de ligne de commande permet à vos scripts Python de prendre des données directement depuis le terminal. Les programmeurs utilisent cela pour rendre les scripts plus flexibles et interactifs.

## How to (Comment faire)
Python utilise le module 'sys' pour accéder aux arguments de ligne de commande.

```Python
import sys

# Exemple de base pour afficher les arguments de ligne de commande
if len(sys.argv) > 1:
    print(f"Arguments: {sys.argv[1:]}")
else:
    print("Aucun argument détecté.")

# Lancer ce script en ligne de commande avec: python script.py arg1 arg2
```

Sortie prévue si vous passez des arguments:
```
Arguments: ['arg1', 'arg2']
```

## Deep Dive (Plongeon en Profondeur)
À l'origine, la lecture des arguments venait du C, utilisant `argc` et `argv`. En Python, `sys.argv` est la liste standard, mais il y a des alternatives plus robustes comme `argparse` ou des bibliothèques tierces comme `click`.

`sys.argv` est direct et simple pour les cas d'utilisation basiques. L'index 0 est le script lui-même. Tout ce qui suit sont des arguments passés par l'utilisateur.

Avec `argparse`, vous pouvez définir des arguments plus sophistiqués, comme les options booléennes et les valeurs par défaut, et même afficher de l'aide.

Voici comment vous pourriez faire avec `argparse`:

```Python
import argparse

# Créer un analyseur
parser = argparse.ArgumentParser(description="Votre script Python.")
parser.add_argument('echos', nargs='*', help="Insérez n'importe quel argument.")
args = parser.parse_args()

# Utiliser les arguments
print(f"Arguments: {args.echos}")
```

## See Also (Voir Aussi)
Pour approfondir et découvrir diverses stratégies de traitement des arguments de ligne de commande en Python, je vous suggère de consulter :

- Documentation Python sur sys.argv: https://docs.python.org/3/library/sys.html#sys.argv
- Documentation Python sur argparse: https://docs.python.org/3/library/argparse.html
- Documentation Click: https://click.palletsprojects.com/en/7.x/ 
- Un bon tutorial sur Real Python pour argparse: https://realpython.com/command-line-interfaces-python-argparse/
