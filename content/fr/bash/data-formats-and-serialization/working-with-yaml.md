---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:41.373824-07:00
description: "YAML, qui signifie \"YAML Ain't Markup Language\" (YAML n'est pas un\
  \ langage de balisage), est un standard de s\xE9rialisation de donn\xE9es lisible\
  \ par l'homme\u2026"
lastmod: '2024-03-13T22:44:58.019581-06:00'
model: gpt-4-0125-preview
summary: "YAML, qui signifie \"YAML Ain't Markup Language\" (YAML n'est pas un langage\
  \ de balisage), est un standard de s\xE9rialisation de donn\xE9es lisible par l'homme\u2026"
title: Travailler avec YAML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

YAML, qui signifie "YAML Ain't Markup Language" (YAML n'est pas un langage de balisage), est un standard de sérialisation de données lisible par l'homme qui peut être utilisé pour des fichiers de configuration, ainsi que dans des applications où les données sont stockées ou transmises. Les programmeurs se tournent vers YAML en raison de sa clarté et de sa simplicité, en particulier dans les projets impliquant des configurations complexes ou le besoin de structures de données facilement modifiables.

## Comment :

Travailler directement avec YAML en Bash requiert un peu d'ingéniosité puisque Bash n'a pas de support intégré pour l'analyse de YAML. Cependant, vous pouvez utiliser des outils externes comme `yq` (un processeur YAML en ligne de commande léger et portable) pour interagir efficacement avec les fichiers YAML. Passons en revue quelques opérations courantes :

### Installer `yq` :

Avant de plonger dans les exemples, assurez-vous d'avoir `yq` installé. Vous pouvez généralement l'installer à partir de votre gestionnaire de paquets, par exemple, sur Ubuntu :

```bash
sudo apt-get install yq
```

Ou vous pouvez le télécharger directement depuis son dépôt GitHub.

### Lire une valeur :

Considérez que vous avez un fichier nommé `config.yaml` avec le contenu suivant :

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secret
```

Pour lire l'hôte de la base de données, vous pouvez utiliser `yq` comme suit :

```bash
yq e '.database.host' config.yaml
```

**Exemple de sortie :**

```
localhost
```

### Mettre à jour une valeur :

Pour mettre à jour le nom de l'utilisateur dans `config.yaml`, utilisez la commande `yq eval` avec l'option `-i` (sur place) :

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

Vérifiez le changement avec :

```bash
yq e '.user.name' config.yaml
```

**Exemple de sortie :**

```
newadmin
```

### Ajouter un nouvel élément :

Pour ajouter un nouvel élément sous la section de la base de données, comme un nouveau champ `timeout` :

```bash
yq e '.database.timeout = 30' -i config.yaml
```

Vérifier le contenu du fichier confirmera l'ajout.

### Supprimer un élément :

Pour supprimer le mot de passe sous utilisateur :

```bash
yq e 'del(.user.password)' -i config.yaml
```

Cette opération supprimera le champ mot de passe de la configuration.

Rappelez-vous, `yq` est un outil puissant et a beaucoup plus de capacités, y compris la conversion de YAML en JSON, la fusion de fichiers, et même des manipulations plus complexes. Référez-vous à la documentation de `yq` pour une exploration plus approfondie.
