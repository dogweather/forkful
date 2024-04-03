---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:14.085304-07:00
description: "V\xE9rifier si un r\xE9pertoire existe dans le Fish Shell permet aux\
  \ scripts de prendre des d\xE9cisions bas\xE9es sur la pr\xE9sence ou l'absence\
  \ de structures de\u2026"
lastmod: '2024-03-13T22:44:58.340004-06:00'
model: gpt-4-0125-preview
summary: "V\xE9rifier si un r\xE9pertoire existe dans le Fish Shell permet aux scripts\
  \ de prendre des d\xE9cisions bas\xE9es sur la pr\xE9sence ou l'absence de structures\
  \ de r\xE9pertoires, facilitant des t\xE2ches comme les op\xE9rations conditionnelles\
  \ sur les fichiers, les journaux d'activit\xE9 ou la configuration de l'environnement."
title: "V\xE9rifier si un r\xE9pertoire existe"
weight: 20
---

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
