---
title:                "Bash: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes un programmeur Bash, vous avez probablement déjà entendu parler de YAML, mais vous vous demandez peut-être pourquoi vous devriez l'utiliser. Eh bien, YAML est un format de données facile à lire et à écrire qui est souvent utilisé pour stocker des configurations et des données structurées. Il est également très utile pour automatiser des tâches en utilisant des scripts Bash.

## Comment faire
Il est facile de travailler avec YAML en utilisant Bash. Tout d'abord, vous devez installer un parseur YAML en utilisant la commande suivante :

```Bash
sudo apt-get install libyaml-dev python-yaml
```

Ensuite, vous pouvez écrire vos données YAML dans un fichier tel que "config.yaml" en utilisant un éditeur de texte de votre choix. Voici un exemple de code YAML :

```Bash
# Configuration pour un serveur web
server:
  port: 8080
  name: Mon serveur web
  ip: 192.168.1.1
  log_level: debug
```

Pour lire ces données YAML dans votre script Bash, vous pouvez utiliser la commande suivante :

```Bash
#!/bin/bash
server_port=$(python -c "import yaml; print(yaml.load(open('config.yaml'))['server']['port'])")
echo "Le port du serveur est $server_port"
```

Lorsque vous exécutez ce script, vous devriez voir la sortie suivante :

```
Le port du serveur est 8080
```

## Plongée en profondeur
Maintenant que vous savez comment travailler avec YAML en Bash, voici quelques conseils pour rendre votre vie plus facile :

- Utilisez des outils tels que "yq" ou "jq" pour manipuler facilement vos données YAML.
- Utilisez un modèle de script pour éviter de copier-coller du code YAML dans chaque script.
- Assurez-vous de valider vos fichiers YAML avant de les utiliser pour éviter les erreurs de syntaxe.

Voici quelques ressources utiles pour en savoir plus sur YAML et Bash :

- [Documentation YAML](https://yaml.org/)
- [Documentation Bash](https://www.gnu.org/software/bash/)
- [yq : un outil en ligne de commande pour manipuler des données YAML](https://mikefarah.gitbook.io/yq/)
- [jq : un outil en ligne de commande pour manipuler des données JSON et YAML](https://stedolan.github.io/jq/)
- [Un modèle de script Bash pour travailler avec YAML](https://gist.github.com/pkuczynski/8665367)

## Voir aussi
- [Utiliser YAML pour stocker des données de configuration en Bash](https://codereviewvideos.com/blog/how-to-use-yaml-to-store-your-bash-script-configuration/)
- [Comment travailler avec YAML en Bash](https://blog.apcelent.com/yaml-bash.html)