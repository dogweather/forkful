---
title:                "Vérifier si un répertoire existe"
date:                  2024-02-03T19:07:14.085304-07:00
model:                 gpt-4-0125-preview
simple_title:         "Vérifier si un répertoire existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Vérifier si un répertoire existe dans le Fish Shell permet aux scripts de prendre des décisions basées sur la présence ou l'absence de structures de répertoires, facilitant des tâches comme les opérations conditionnelles sur les fichiers, les journaux d'activité ou la configuration de l'environnement. Cette technique est cruciale pour écrire des scripts robustes qui interagissent avec le système de fichiers de manière prévisible.

## Comment faire :
Le Fish Shell utilise la commande `test` pour vérifier les types de fichiers et leurs caractéristiques, y compris si une cible est un répertoire. Voici un modèle de base pour vérifier si un répertoire existe :

```fish
if test -d /chemin/vers/rep
    echo "Le répertoire existe"
else
    echo "Le répertoire n'existe pas"
end
```
Exemple de sortie :
```
Le répertoire existe
```

Pour des opérations de fichiers et de répertoires plus rationalisées, on pourrait se tourner vers des outils externes comme `fd`, bien qu'il soit plus couramment utilisé pour trouver des fichiers et des répertoires plutôt que de juste vérifier leur existence. Cependant, le combiner avec des scripts Fish peut donner des résultats pratiques :

```fish
set dir "/chemin/vers/la/recherche"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "Le répertoire existe"
else
    echo "Le répertoire n'existe pas"
end
```

Cet exemple `fd` recherche le répertoire à une profondeur spécifiée, et `grep` vérifie la correspondance, le rendant versatile pour des vérifications nuancées. Cependant, pour le but direct de vérifier l'existence, se tenir à la commande `test` intégrée de Fish est à la fois efficace et simple.
