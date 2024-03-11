---
date: 2024-01-27 16:21:18.345278-07:00
description: "Imaginez que vous venez de d\xE9couvrir que vous devez effectuer une\
  \ mise \xE0 jour par lots sur plusieurs fichiers de configuration se trouvant sur\
  \ votre\u2026"
lastmod: '2024-03-11T00:14:31.919271-06:00'
model: gpt-4-0125-preview
summary: "Imaginez que vous venez de d\xE9couvrir que vous devez effectuer une mise\
  \ \xE0 jour par lots sur plusieurs fichiers de configuration se trouvant sur votre\u2026"
title: "Modification de fichiers sur place avec des lignes de commande en une seule\
  \ \xE9tape"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Imaginez que vous venez de découvrir que vous devez effectuer une mise à jour par lots sur plusieurs fichiers de configuration se trouvant sur votre serveur. Vous pourriez ouvrir chaque fichier, apporter les modifications manuellement et les enregistrer. Ou bien, vous pouvez effectuer une modification sur place directement depuis votre interface de ligne de commande (CLI), une compétence qui permet de gagner du temps, de réduire les erreurs et d'automatiser les tâches répétitives. Cette technique est particulièrement utile pour les mises à jour systémiques, les corrections ou les modifications en masse où les modifications manuelles pourraient être impraticables ou sujettes à erreur.

## Comment procéder :

Lorsqu'il s'agit de modifier des fichiers sur place en utilisant Bash, deux outils prédominants entrent en jeu : `sed` et `awk`. Explorons comment utiliser ces utilitaires puissants avec quelques exemples de code.

### Utiliser `sed` pour un simple remplacement de texte

La commande suivante remplace la première occurrence de "text1" par "text2" dans `file.txt` :

```Bash
sed -i 's/text1/text2/' file.txt
```

Pour un remplacement global (toutes les occurrences), vous ajouteriez un `g` à la fin :

```Bash
sed -i 's/text1/text2/g' file.txt
```

Pour modifier plusieurs fichiers à la fois :

```Bash
sed -i 's/text1/text2/g' file1.txt file2.txt file3.txt
```

### Utiliser `awk` pour des manipulations plus complexes

`awk` est un autre outil qui brille par ses capacités de programmation, particulièrement utiles pour le traitement de texte impliquant des données basées sur des champs.

Changer le deuxième champ de chaque ligne en `newValue` dans `data.csv`, séparées par des virgules :

```Bash
awk -i inplace -F, '{$2="newValue"; print $0}' OFS=, data.csv
```

### Sauvegardez avant de vous lancer

Un conseil pratique : créez toujours une sauvegarde avant de modifier sur place. `sed` facilite cela avec l'option `-i` suivie d'un suffixe pour créer une sauvegarde.

```Bash
sed -i.bak 's/text1/text2/g' file.txt
```

Cette commande crée une sauvegarde de l'original `file.txt` sous `file.txt.bak` avant d'effectuer le remplacement.

## Approfondissement

La capacité de modifier des fichiers directement depuis la ligne de commande est apparue comme une progression naturelle de la philosophie Unix : donner aux utilisateurs le pouvoir de gérer et manipuler les données efficacement avec le moins de frappes possible. Cependant, ce pouvoir comporte ses mises en garde.

### Contexte historique

Les outils Unix tels que `sed` et `awk` existent depuis les premiers jours d'Unix, conçus comme partie intégrante de sa philosophie d'outillage, axée sur des commandes spécialisées et composables. Leur inclusion dans l'arsenal d'Unix a été une réponse au besoin de traitement de texte efficace dans un paysage dominé par les interfaces de ligne de commande.

### Alternatives

Bien que `sed` et `awk` soient puissants, ils ne sont pas les seules options. Perl et Python, par exemple, disposent d'options de ligne de commande (`-p` et `-i`, respectivement) qui permettent des capacités d'édition sur place similaires avec une syntaxe potentiellement plus lisible pour des opérations complexes.

```Bash
perl -pi -e 's/text1/text2/g' file.txt
```

```Bash
python -c "import fileinput, sys; [sys.stdout.write(line.replace('text1', 'text2')) for line in fileinput.input(files='file.txt', inplace=True)]"
```

Chaque alternative a ses forces : les capacités en ligne de Perl sont immenses, et la syntaxe de Python est sans doute plus accessible à ceux qui ne sont pas profondément versés dans les outils de traitement de texte Unix.

### Détails de mise en œuvre

L'édition sur place n'est pas véritablement "sur place" au sens technique. `sed -i` et `awk -i inplace` fonctionnent en créant un fichier temporaire dans lequel la sortie traitée est stockée avant de remplacer le fichier original. Cette approche garantit que le fichier n'est pas corrompu si le processus est interrompu. Les implications concernent principalement les ressources et les permissions : vous devez disposer de suffisamment d'espace disque pour le fichier temporaire et des permissions pour créer des fichiers dans le répertoire de votre fichier cible.

Bien que puissantes, les commandes d'édition sur place doivent être utilisées avec prudence. Une expression régulière mal placée peut entraîner une perte de données, ce qui souligne l'importance des sauvegardes. Malgré les pièges potentiels, maîtriser ces commandes peut considérablement améliorer votre capacité à effectuer des modifications de fichiers rapides et efficaces directement depuis la ligne de commande, incarnant la philosophie Unix d'utiliser des outils simples et puissants pour accomplir des tâches complexes.
