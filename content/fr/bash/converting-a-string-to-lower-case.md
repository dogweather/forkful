---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La conversion d'une chaîne en minuscules est le processus de conversion de tous les caractères alphanumériques d'une chaîne en minuscules. Les programmeurs le font souvent pour assurer uniformité et cohérence dans le traitement de l'information textuelle.

## Comment faire:

Voici comment vous pouvez convertir une chaîne en minuscules en bash:

```Bash
chaine="JE SUIS EN MAJUSCULES"
chaine_en_minuscules=$(echo "$chaine" | tr '[:upper:]' '[:lower:]')

echo "$chaine_en_minuscules"
```

Lorsque vous exécutez ce script, la sortie sera:

```Bash
je suis en minuscules
```

## Exploration approfondie:

Historiquement, la conversion de chaînes en minuscules est une opération fréquente dans le traitement du texte dans de nombreuses applications, y compris la recherche de texte, le tri, l'analyse de données et plus encore.

Il existe plusieurs façons de faire cette conversion en bash. Une alternative à l'exemple donné serait d'utiliser la commande `awk`:

```Bash
chaine_en_minuscules=$(echo "$chaine" | awk '{print tolower($0)}')
```

La commande `tr` utilisée dans notre exemple est une commande UNIX standard qui sert à traduire ou supprimer des caractères. En interne, il utilise une table de correspondance pour effectuer la conversion entre les caractères majuscules et minuscules.

## Voir Aussi:

Voici quelques sources utiles pour approfondir le sujet:

- GNU Coreutils: [Tr](https://www.gnu.org/software/coreutils/manual/coreutils.html#tr-invocation)
- GNU Awk User’s Guide: [Built-in Functions](https://www.gnu.org/software/gawk/manual/html_node/Built_002din-Functions.html)
- Advanced Bash-Scripting Guide: [Case Conversion](http://www.tldp.org/LDP/abs/html/case-conversion.html)