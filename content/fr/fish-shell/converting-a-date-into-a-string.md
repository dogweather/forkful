---
title:                "Conversion d'une date en chaîne de caractères"
html_title:           "Fish Shell: Conversion d'une date en chaîne de caractères"
simple_title:         "Conversion d'une date en chaîne de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Convertir une date en une chaîne de caractères signifie prendre une date sous forme de données et la transformer en une représentation textuelle. Les programmeurs le font pour faciliter la lecture et la manipulation des dates dans leur code.

## Comment faire:

Utilisez la fonction `strftime` pour convertir une date en une chaîne de caractères dans le Fish Shell. Voici un exemple de code:

```
set date (date -T)
echo "Aujourd'hui, nous sommes le" (strftime "%A %d %B %Y" $date)
```

Cela va produire une sortie comme suit:

```
Aujourd'hui, nous sommes le jeudi 02 septembre 2021
```

Vous pouvez également utiliser `date -f` pour formatter une chaîne de caractères en une date. Voici un exemple:

```
set date (date -f "%F" "2021-09-02")
echo "Aujourd'hui, c'est le" (strftime "%A" $date)
```

Cela va produire une sortie comme suit:

```
Aujourd'hui, c'est le jeudi
```

## Plongée en profondeur:

La conversion d'une date en une chaîne de caractères est souvent nécessaire pour afficher ou enregistrer des dates dans différents formats. Avant les outils informatiques modernes, les dates étaient souvent stockées sous forme de chaînes de caractères pour simplifier leur manipulation. Cependant, avec l'avènement des systèmes de date et d'heure standardisés, le besoin de cette conversion a diminué.

Il existe différentes méthodes pour convertir une date en une chaîne de caractères dans le Fish Shell. En plus de `strftime`, vous pouvez également utiliser `date -i` pour convertir une chaîne de caractères en une date en utilisant un format spécifié par l'utilisateur.

## Voir aussi:

Pour en savoir plus sur la conversion de dates en chaînes de caractères dans le Fish Shell, consultez la [documentation officielle de Fish Shell](https://fishshell.com/docs/current/cmds/date.html) et le [guide de référence de Fish Shell](https://fishshell.com/docs/current/index.html).