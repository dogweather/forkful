---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## C'est quoi & Pourquoi ?

La recherche et le remplacement de texte sont des fonctions qui permettent de localiser certaines chaînes de caractères dans un texte et de les remplacer par d'autres. Les programmeurs le font pour modifier les données, manipuler des fichiers de code et améliorer l'efficacité.

## Comment faire :

Pour ce faire, on utilise principalement la commande `sed` dans Bash. `sed` est un éditeur de flux permettant de manipuler le texte.

```Bash
# Pour remplacer 'ancient' par 'nouveau' dans un fichier
sed 's/ancient/nouveau/' file.txt
```

Sortie attendue :

```Bash
{nouveau texte remanié}
```

Et pour remplacer globalement dans le fichier, on utilise l'option 'g' :

```Bash
# Pour remplacer toutes les occurrences de 'ancient' par 'nouveau' dans un fichier
sed 's/ancient/nouveau/g' file.txt
```

Sortie attendue :

```Bash
{nouveau texte remanié avec toutes les occurrences remplacées}
```

## Plongée profonde :

Historiquement, `sed` était largement utilisé pour la manipulation du texte dans les pipelines Unix dès les années 1970. C'est un outil puissant qui utilise des expressions régulières pour la correspondance et le remplacement de texte.

Il existe d'autres alternatives pour rechercher et remplacer le texte en Bash, comme `awk` ou `perl -p -i -e`, mais `sed` est largement considéré comme le moyen le plus simple et le plus élégant de le faire.

La commande `sed` fonctionne en lisant le fichier, en recherchant le texte à remplacer, puis en effectuant le remplacement. L'option 'g' indique que le remplacement doit être fait partout dans le fichier et pas seulement à la première occurrence.

## Voir aussi :

Pour approfondir la recherche et le remplacement de texte en Bash, vous pouvez consulter ces ressources :

- [GNU Sed Documentation](https://www.gnu.org/software/sed/manual/sed.html)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial#h8-the-sed-stream-editor)