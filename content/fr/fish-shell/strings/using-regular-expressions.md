---
aliases:
- /fr/fish-shell/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:48.974513-07:00
description: "Les expressions r\xE9guli\xE8res (regex) dans Fish Shell permettent\
  \ de rechercher, d'apparier et de manipuler des cha\xEEnes de caract\xE8res bas\xE9\
  es sur des motifs\u2026"
lastmod: 2024-02-18 23:09:09.292417
model: gpt-4-0125-preview
summary: "Les expressions r\xE9guli\xE8res (regex) dans Fish Shell permettent de rechercher,\
  \ d'apparier et de manipuler des cha\xEEnes de caract\xE8res bas\xE9es sur des motifs\u2026"
title: "Utilisation des expressions r\xE9guli\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les expressions régulières (regex) dans Fish Shell permettent de rechercher, d'apparier et de manipuler des chaînes de caractères basées sur des motifs spécifiques. Les programmeurs utilisent les regex pour des tâches telles que la validation d'entrée, l'analyse syntaxique et le traitement de texte, car cela offre un moyen compact et puissant de spécifier des motifs de texte complexes.

## Comment faire :

Bien que Fish Shell lui-même ne dispose pas d'une commande intégrée pour les regex, il utilise efficacement des commandes externes comme `grep`, `sed` et `awk` qui prennent en charge les regex, vous permettant d'incorporer des opérations regex dans vos scripts.

### Appariement de motifs basique avec `grep`
Recherchez des lignes dans un fichier qui correspondent à un motif :

```fish
grep '^[0-9]+' myfile.txt
```

Cette commande trouve les lignes commençant par un ou plusieurs chiffres dans `myfile.txt`.

### Extraction et remplacement avec `sed`
Extrayez les numéros de téléphone d'un fichier :

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

Remplacez toutes les occurrences de "foo" par "bar" dans `data.txt` :

```fish
sed 's/foo/bar/g' data.txt
```

### Utilisation de `string` pour les Regex basiques
La commande `string` de Fish Shell prend en charge les opérations regex simples comme l'appariement et le remplacement :

Appariez un motif dans une chaîne :

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
Sortie :
```
3.1.2
```

Remplacez les chiffres suivant 'fish' par 'X.X.X' :

```fish
echo "Bienvenue dans fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
Sortie :
```
Bienvenue dans fish X.X.X
```

### Appariement avancé avec `awk`
Imprimez la deuxième colonne de données où la première colonne correspond à un motif spécifique :

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

Cette commande recherche dans `datafile` les lignes où la première colonne commence par un "a" suivi d'un ou plusieurs chiffres et imprime la deuxième colonne.

En intégrant ces commandes externes, les programmeurs de Fish Shell peuvent exploiter toute la puissance des expressions régulières pour des tâches complexes de manipulation de texte, améliorant ainsi les capacités natives du shell.
