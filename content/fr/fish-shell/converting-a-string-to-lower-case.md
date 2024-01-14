---
title:                "Fish Shell: Convertir une chaîne en minuscules"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Convertir une chaîne de caractères en minuscules peut être utile pour normaliser les données saisies par l'utilisateur ou pour faciliter les comparaisons de chaînes de caractères. Avec Fish Shell, cela peut être fait en quelques lignes de code simples.

## Comment Faire

```Fish Shell
set string "Bonjour LE Monde!"
echo $string | tr '[:upper:]' '[:lower:]'
```

Output:

```
bonjour le monde!
```

Le code ci-dessus utilise la commande `tr` pour convertir toutes les lettres majuscules en lettres minuscules dans la chaîne de caractères donnée. Si vous souhaitez conserver la chaîne de caractères d'origine, vous pouvez utiliser la commande `set` pour stocker la chaîne dans une variable.

## Plongée Profonde

La commande `tr` utilise des règles de traduction pour effectuer ses conversions. Vous pouvez spécifier des règles personnalisées en utilisant la syntaxe suivante:

```Fish Shell
echo $string | tr 'a-z' 'A-Z'
```

Ici, nous utilisons la plage de caractères `a-z` pour spécifier que toutes les lettres minuscules doivent être converties en lettres majuscules. Vous pouvez également utiliser des caractères spéciaux pour remplir des conditions plus complexes de conversion.

La commande `tr` peut également être utilisée pour supprimer des caractères de la chaîne d'origine. Par exemple, pour supprimer tous les chiffres de la chaîne, vous pouvez utiliser:

```Fish Shell
echo $string | tr -d '[0-9]'
```

Pour plus d'informations sur les règles de traduction et les options de la commande `tr`, vous pouvez consulter sa page de manuel en tapant `man tr` dans le terminal.

## Voir aussi

- [Tutoriel Fish Shell en français](https://fishshell.com/docs/current/tutorial.html)
- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/)
- [Chaînes de caractères avec Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-strings)