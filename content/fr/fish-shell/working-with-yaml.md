---
title:                "Fish Shell: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur ou un administrateur système, vous savez probablement à quel point il est important d'automatiser certaines tâches pour gagner du temps et garantir la cohérence dans votre travail. C'est là que YAML entre en jeu. YAML est un langage de sérialisation de données facile à lire et à écrire, principalement utilisé pour la configuration, ce qui en fait un outil indispensable pour automatiser vos tâches quotidiennes.

## Comment faire

La première étape pour travailler avec YAML dans Fish Shell est d'installer le plugin fish-pe (Fisher + PE), qui fournit les fonctionnalités nécessaires pour manipuler les données YAML. Une fois installé, il suffit d'utiliser la commande `yqr` pour lire un fichier YAML et la commande `yqw` pour écrire dans un fichier YAML. Voici un exemple de code pour lire et écrire dans un fichier YAML :

```Fish Shell
# Lecture d'un fichier YAML
set config (yqr config.yaml)

# Affichage des données
echo $config

# Modification des données
set config.name "John Doe"
set config.age 30

# Écriture dans le fichier YAML
yqw config.yaml $config
```

Lors de l'exécution de ce code, vous verrez le contenu du fichier YAML affiché dans votre terminal, puis les données seront modifiées et réécrites dans le même fichier.

## Plongée en profondeur

Maintenant que vous savez comment travailler avec YAML dans Fish Shell, voici quelques astuces pour tirer le meilleur parti de cet outil : 

- Utilisez des variables pour stocker les données YAML, ce qui facilite la modification et la réutilisation de ces données dans votre code.
- Utilisez la commande `yqr -e` pour extraire une valeur spécifique d'un fichier YAML, ce qui peut être utile pour automatiser des tâches telles que la création de dossiers ou de fichiers.
- Consultez la documentation complète de fish-pe pour en savoir plus sur les fonctionnalités avancées de manipulation de données YAML.

## Voir aussi

- [Site officiel de Fish Shell](https://fishshell.com/)
- [Documentation de fish-pe](https://github.com/fisherman/pe)
- [Site officiel de YAML](https://yaml.org/)