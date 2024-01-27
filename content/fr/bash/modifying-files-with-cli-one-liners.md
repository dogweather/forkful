---
title:                "Modification de fichiers avec des commandes en une ligne en CLI"
date:                  2024-01-26T22:19:01.007242-07:00
model:                 gpt-4-0125-preview
simple_title:         "Modification de fichiers avec des commandes en une ligne en CLI"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/modifying-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Modifier des fichiers avec des one-liners CLI (Interface de Ligne de Commande) consiste à apporter des modifications rapides et ciblées aux fichiers directement depuis votre terminal. Les programmeurs le font parce que c'est rapide, scriptable et, lorsqu'ils travaillent dans des environnements comme Linux, c'est souvent la manière la plus directe d'appliquer des modifications sans ouvrir un éditeur réel. Cela tire parti de la puissance de sed, awk, grep et d'autres outils en ligne de commande pour rechercher, remplacer, insérer ou supprimer le contenu des fichiers à la volée.

## Comment faire :

Passons en revue quelques exemples de base :

1. **Remplacer du texte** dans un fichier en utilisant `sed` :
   ```Bash
   sed -i 's/oldText/newText/g' nomdefichier.txt
   ```
   Cette commande recherche `oldText` dans `nomdefichier.txt` et le remplace par `newText`.

2. **Ajouter du texte** à la fin d'un fichier :
   ```Bash
   echo "Nouvelle ligne de texte" >> nomdefichier.txt
   ```
   Ajoute une nouvelle ligne de texte à la fin de `nomdefichier.txt`.

3. **Supprimer une ligne** contenant une chaîne spécifique avec `sed` :
   ```Bash
   sed -i '/chaîneÀSupprimer/d' nomdefichier.txt
   ```
   Supprime les lignes contenant `chaîneÀSupprimer` de `nomdefichier.txt`.

4. **Extraire et afficher** les lignes correspondant à un motif en utilisant `grep` :
   ```Bash
   grep 'motifÀRechercher' nomdefichier.txt
   ```
   Affiche les lignes de `nomdefichier.txt` qui correspondent au motif.

## Approfondissement

Modifier des fichiers en utilisant des one-liners CLI est une technique aussi ancienne qu'Unix lui-même, reposant largement sur des outils comme `sed`, `awk`, `grep` et `cut`. Ces utilitaires ont été conçus aux premiers jours d'Unix pour gérer efficacement les tâches de traitement de texte, exploitant le concept de pipeline alors révolutionnaire.

**Alternatives** : Bien que ces one-liners soient puissants, ils ont des limitations, notamment lorsqu'il s'agit de traiter des structures de données plus complexes ou des fichiers binaires. Dans de tels cas, des langages de script de haut niveau comme Python ou Perl pourraient être plus appropriés en raison de leurs capacités avancées de parsing et de manipulation de données.

**Détails d'implémentation** : Comprendre les expressions régulières (regex) est crucial lorsqu'on travaille avec ces outils, car elles constituent la base de la correspondance de motifs et de la manipulation de texte. De plus, l'option `-i` avec `sed` pour l'édition sur place ne fonctionne pas de manière universelle sur tous les systèmes de la même manière, en particulier sur macOS par rapport à Linux, où vous devrez peut-être inclure un argument pour l'extension de sauvegarde avec `-i` sur macOS.

## Voir aussi

- Manuel GNU `sed` : [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- Le langage de programmation AWK : [https://www.cs.princeton.edu/~bwk/btl.mirror/](https://www.cs.princeton.edu/~bwk/btl.mirror/)
- Page du manuel Grep : [https://www.gnu.org/software/grep/manual/grep.html](https://www.gnu.org/software/grep/manual/grep.html)
- Informations sur les expressions régulières : [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
