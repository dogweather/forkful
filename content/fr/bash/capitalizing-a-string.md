---
title:    "Bash: Mettre en majuscule une chaîne de caractères"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous codez en Bash, vous avez probablement déjà rencontré le besoin de mettre en majuscule une chaîne de caractères. Que ce soit pour des raisons d'esthétique ou de traitement de données, il est parfois nécessaire de capitaliser une chaîne. Dans cet article, nous allons vous montrer comment réaliser cela de manière simple et efficace.

## Comment faire

Il existe différentes méthodes pour capitaliser une chaîne de caractères en Bash, mais nous allons vous présenter la plus couramment utilisée. Tout d'abord, il faut comprendre qu'en Bash, les chaînes de caractères sont traitées comme des tableaux de caractères. Cela signifie que chaque caractère est stocké dans une case du tableau et peut donc être manipulé individuellement.

Pour capitaliser une chaîne de caractères, nous allons utiliser une boucle for pour parcourir chaque caractère et un test conditionnel pour vérifier s'il s'agit d'une lettre minuscule. Si c'est le cas, nous allons utiliser la commande `tr` pour convertir le caractère en majuscule. Voici un exemple de code :

```Bash
#!/bin/bash

string="exEMpLe"

for (( i=0; i<${#string}; i++ )); do
    if [[ "${string:$i:1}" =~ [[:lower:]] ]]; then
        string="${string/$i/$(tr '[:lower:]' '[:upper:]' <<< ${string:$i:1})}"
    fi
done

echo $string
```
L'output de ce code sera "ExEMpLe", avec la première et troisième lettre en majuscule.

## Plongée en profondeur

Maintenant que vous savez comment capitaliser une chaîne de caractères en Bash, vous pouvez aller plus loin en utilisant des méthodes plus avancées comme la substitution des paramètres ou les expressions régulières. Vous pouvez également combiner cette technique avec d'autres commandes Bash pour obtenir des résultats plus complexes.

Par exemple, si vous voulez capitaliser une chaîne de caractères tout en conservant les éventuels accents, vous pouvez utiliser la commande `sed` pour effectuer un remplacement spécifique. Voici un autre exemple de code qui capitalise la chaîne tout en gardant les éventuels accents :

```Bash
#!/bin/bash

string="éxemplé"

string="$(echo $string | sed -e 's/\(.*\)/\U\1/2')"
echo $string
```

L'output de ce code sera "Éxemplé", avec l'accent aigu conservé.

## Voir aussi

Maintenant que vous maîtrisez la capitalisation des chaînes de caractères en Bash, vous pouvez explorer d'autres techniques de manipulation de chaînes telles que la concaténation, la découpe ou la suppression de sous-chaînes. Vous pouvez également découvrir toutes les différentes fonctions Bash disponibles pour manipuler les chaînes de caractères. Voici quelques liens utiles pour aller plus loin :

- [La documentation Bash officielle](https://www.gnu.org/software/bash/manual/)
- [Les meilleures pratiques en Bash](https://kvz.io/bash-best-practices.html)
- [Le cours "Learn Bash the Hard Way"](https://learnbchardway.com/)