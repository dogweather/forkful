---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:36.638367-07:00
description: "L'analyse d'une date \xE0 partir d'une cha\xEEne en Bash consiste \xE0\
  \ extraire et convertir les informations de date \xE0 partir de donn\xE9es textuelles\
  \ en un format\u2026"
lastmod: 2024-02-19 22:05:16.714044
model: gpt-4-0125-preview
summary: "L'analyse d'une date \xE0 partir d'une cha\xEEne en Bash consiste \xE0 extraire\
  \ et convertir les informations de date \xE0 partir de donn\xE9es textuelles en\
  \ un format\u2026"
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

L'analyse d'une date à partir d'une chaîne en Bash consiste à extraire et convertir les informations de date à partir de données textuelles en un format que Bash peut manipuler ou utiliser pour des processus ultérieurs. C'est une exigence courante dans le scripting pour des tâches telles que l'analyse de fichiers journaux, l'organisation de fichiers basée sur des timbres de date, ou la création de rapports automatisés, rendant cela une compétence essentielle pour les programmeurs afin de gérer et d'utiliser efficacement les données temporelles.

## Comment faire :

Bash lui-même est assez limité en capacités de parsing de date directes, s'appuyant souvent sur des outils externes comme `date` et `awk` pour une manipulation plus sophistiquée. Voici comment vous pouvez analyser un format spécifique puis l'utiliser avec la commande `date` pour le convertir ou effectuer des opérations.

**Exemple 1 :** Extraire une chaîne de date et la convertir dans un autre format.

Supposons que vous avez une date au format `aaaa-mm-jj` et que vous souhaitez la convertir en `jj-mm-aaaa`.

```bash
date_originale="2023-04-01"
date_formatee=$(date -d $date_originale '+%d-%m-%Y')

echo $date_formatee
```

**Exemple de sortie :**
```
01-04-2023
```

Cela utilise la commande `date` avec l'option `-d` pour spécifier la chaîne de date d'entrée, et `+%d-%m-%Y` pour formater la sortie.

**Exemple 2 :** Utilisation de `awk` pour analyser une date à partir d'une ligne de texte structuré et la convertir.

Supposant que vous avez une ligne de fichier journal :

```
2023-04-01 12:00:00 Utilisateur connecté
```

Vous pouvez extraire et convertir la partie date en utilisant `awk` et `date`.

```bash
ligne_journal="2023-04-01 12:00:00 Utilisateur connecté"
partie_date=$(echo $ligne_journal | awk '{print $1}')
date_formatee=$(date -d $partie_date "+%A, %B %d, %Y")

echo $date_formatee
```

**Exemple de sortie :**
```
Samedi, avril 01, 2023
```

Cet exemple utilise `awk` pour diviser la ligne de journal et extraire la partie date (`$1` représente le premier champ délimité par un espace), puis `date` est utilisé pour le reformatage.

### Utilisation d'outils tiers

Pour un parsing plus complexe ou lorsqu'on traite une grande variété de formats de date, les outils tiers comme `dateutils` peuvent être très pratiques.

**Exemple avec `dateutils` :**

Supposant que vous avez une chaîne de date dans un format non standard, par exemple, `avril 01, 2023`.

```bash
date_originale="avril 01, 2023"
date_formatee=$(dateconv -i "%B %d, %Y" -f "%Y-%m-%d" <<< $date_originale)

echo $date_formatee
```

**Exemple de sortie :**
```
2023-04-01
```

Cette commande utilise `dateconv` de `dateutils`, spécifiant le format d'entrée avec `-i` et le format de sortie souhaité avec `-f`. `dateutils` prend en charge une vaste gamme de formats de date et d'heure, rendant cet outil très polyvalent pour les tâches de parsing de date dans les scripts Bash.
