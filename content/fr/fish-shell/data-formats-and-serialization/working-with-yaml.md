---
title:                "Travailler avec YAML"
aliases:
- /fr/fish-shell/working-with-yaml.md
date:                  2024-02-03T19:25:24.654262-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec YAML implique l'analyse et la manipulation de fichiers YAML (YAML Ain't Markup Language), un format de sérialisation de données utilisé pour les fichiers de configuration, dans Fish Shell. Les programmeurs font cela pour automatiser et configurer des applications ou des services de manière efficace dans le contexte des environnements shell, facilitant des tâches comme la gestion des configurations et le provisioning des ressources.

## Comment faire :
Fish Shell ne dispose pas de support intégré pour l'analyse de YAML, mais vous pouvez utiliser des outils tiers comme `yq` (un processeur YAML léger et portable en ligne de commande) pour manipuler les données YAML.

**Installation de yq (si pas déjà installé) :**
```fish
sudo apt-get install yq
```

**Lire une valeur d'un fichier YAML :**
Supposons que vous ayez un fichier YAML `config.yaml` avec le contenu suivant :
```yaml
database:
  host: localhost
  port: 3306
```

Pour lire l'hôte de la base de données, vous utiliseriez :
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**Exemple de sortie :**
```
localhost
```

**Mettre à jour une valeur dans un fichier YAML :**
Pour mettre à jour le `port` à `5432`, utilisez :
```fish
yq e '.database.port = 5432' -i config.yaml
```
**Vérifiez la mise à jour :**
```fish
yq e '.database.port' config.yaml
```
**Exemple de sortie :**
```
5432
```

**Écrire un nouveau fichier YAML :**
Pour créer un nouveau `new_config.yaml` avec un contenu prédéfini :
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
Cela utilise `yq` pour traiter et imprimer joliment (-P flag) une chaîne dans un nouveau fichier YAML.

**Analyser des structures complexes :**
Si vous avez un fichier YAML plus complexe et avez besoin de récupérer des tableaux ou des objets imbriqués, vous pouvez :
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**Exemple de sortie :**
```
server1
server2
```
En utilisant `yq`, Fish Shell rend la navigation dans les documents YAML et leur manipulation pour diverses tâches d'automatisation et de configuration simples.
