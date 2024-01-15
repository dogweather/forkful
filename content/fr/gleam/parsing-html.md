---
title:                "Analyse de HTML"
html_title:           "Gleam: Analyse de HTML"
simple_title:         "Analyse de HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi 

Tu sais comment ça se passe : tu dois extraire des informations à partir d'un site Web et tu te retrouves à passer des heures à les copier-coller manuellement. Mais il y a une manière beaucoup plus facile : utiliser Gleam pour analyser automatiquement le HTML !

## Comment faire 

Voici comment utiliser Gleam pour extraire des données en HTML :

```Gleam
let html = "<div><h1>Titre</h1><p>Paragraphe</p></div>"
let parsed = Html.parse(html)

Html.tag(parsed) // "div"
Html.children(parsed) // une liste contenant {"h1", "p"}

```

Et voici le résultat: 

```
Titre
Paragraphe
```

## Plongeon en profondeur 

Maintenant que tu sais comment utiliser Gleam pour extraire les données HTML, voici quelques informations supplémentaires pour te permettre de devenir un pro :

- `Html.tag` te renvoie le nom de la balise du HTML.
- `Html.children` te permet d'accéder aux balises enfants.
- `Html.attributes` te fournit une liste de tous les attributs présents dans le HTML.
- Tu peux également utiliser `Html.find` pour trouver une balise spécifique dans le document HTML.

Et voilà, tu es désormais un expert en matière d'analyse HTML avec Gleam ! Tu peux maintenant dire adieu aux heures passées à copier-coller manuellement les données.

## Voir aussi 

- [Documentation officielle de Gleam](https://gleam.run)
- [Guide complet pour débuter avec Gleam](https://gleam.run/getting-started/Hello-World.html)