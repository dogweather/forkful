---
date: 2024-01-26 04:24:44.909709-07:00
description: "TOML, abr\xE9viation de Tom's Obvious, Minimal Language, est un format\
  \ de s\xE9rialisation de donn\xE9es facile \xE0 lire gr\xE2ce \xE0 sa s\xE9mantique\
  \ claire. Les\u2026"
lastmod: '2024-03-13T22:44:58.085690-06:00'
model: gpt-4-0125-preview
summary: "TOML, abr\xE9viation de Tom's Obvious, Minimal Language, est un format de\
  \ s\xE9rialisation de donn\xE9es facile \xE0 lire gr\xE2ce \xE0 sa s\xE9mantique\
  \ claire."
title: Travailler avec TOML
weight: 39
---

## Quoi et Pourquoi ?

TOML, abréviation de Tom's Obvious, Minimal Language, est un format de sérialisation de données facile à lire grâce à sa sémantique claire. Les programmeurs l'utilisent pour des fichiers de configuration, car il offre un équilibre entre lisibilité humaine et adaptabilité machine.

## Comment :

Dans PowerShell, il n'existe pas de cmdlet natif pour analyser le TOML. Vous utiliseriez typiquement un module ou convertiriez le TOML en JSON avec un outil comme `toml-to-json` si vous voulez travailler avec PowerShell. Voici comment vous le feriez avec un module fictif `PowerShellTOML` :

```PowerShell
# Premièrement, installer le module (imaginaire, pour la démonstration)
Install-Module PowerShellTOML

# Importer un fichier TOML
$config = Import-TomlConfig -Path './config.toml'

# Accéder à une valeur
Write-Output $config.database.server

# Contenu TOML d'exemple dans 'config.toml':
# [database]
# serveur = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connexion_max = 5000

# Sortie d'exemple :
# 192.168.1.1
```

## Plongée profonde

TOML a été créé par Tom Preston-Werner, co-fondateur de GitHub, comme une alternative plus simple à XML et YAML pour les fichiers de configuration. Sa première version est apparue en 2013. TOML est comparable à JSON, mais est conçu pour être plus convivial pour l'humain, ce qui en fait un bon choix pour les configurations maintenues par des personnes. Les alternatives incluent YAML, JSON et XML.

En termes de mise en œuvre, un module PowerShell pour TOML serait typiquement un wrapper autour d'une bibliothèque TOML écrite dans un langage plus orienté performance comme C#. PowerShell n'a pas de support intégré pour TOML, c'est pourquoi un tel module est nécessaire pour interagir commodément avec le format TOML.

## Voir aussi

- Norme TOML : https://toml.io/en/
- Répertoire GitHub pour le module `toml` PowerShell (s'il existe au moment de la lecture) : https://github.com/powershell/PowerShellTOML
- Une introduction à TOML : https://github.com/toml-lang/toml
- Comparaison des formats de sérialisation de données : https://fr.wikipedia.org/wiki/Comparaison_des_formats_de_sérialisation_de_données
