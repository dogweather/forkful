---
title:                "Gleam: Analyse de html"
simple_title:         "Analyse de html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Le balisage HTML est une forme de communication universelle entre les serveurs et les navigateurs, offrant un moyen efficace pour afficher du contenu sur le web. Le langage de programmation Gleam offre une solution robuste pour analyser et extraire des données à partir de ces balises HTML.

## Comment faire

Pour commencer à analyser le HTML dans Gleam, vous aurez besoin d'installer le paquet `gleam_html` à partir de la boutique de paquets Gleam. Ensuite, vous pouvez utiliser la fonction `Html.parse` pour extraire le contenu d'une page web spécifique. Par exemple :

```Gleam
let html = """
<html>
    <head>
        <title>Mon site web</title>
    <head>
    <body>
        <h1>Bienvenue sur mon site web</h1>
        <p>C'est mon premier site utilisant Gleam</p>
    </body>
</html>
"""

Html.parse(html)

// Sortie :
{
    children: [
        {
            tag: "head",
            children: [
                { tag: "title", inner_html: "Mon site web" }
            ]
        },
        {
            tag: "body",
            children: [
                { tag: "h1", inner_html: "Bienvenue sur mon site web" },
                { tag: "p", inner_html: "C'est mon premier site utilisant Gleam" }
            ]
        }
    ]
}
```

## Plongée en profondeur

Outre l'utilisation de la fonction `Html.parse`, vous pouvez également utiliser des sélecteurs CSS pour cibler des éléments spécifiques dans votre HTML. Par exemple, si vous voulez extraire le contenu du paragraphe dans l'exemple précédent, vous pouvez utiliser la fonction `Html.select` comme ceci :

```Gleam
let paragraph = Html.select(html, "p")

// Sortie : "C'est mon premier site utilisant Gleam"
```

Vous pouvez également utiliser des librairies externes comme `Html-to-text` pour convertir le HTML en texte brut afin de faciliter l'analyse et l'extraction de données.

## Voir aussi

- [Documentation officielle de Gleam sur la manipulation de HTML](https://gleam.run/documentation/?api=html)
- [Github du projet Gleam](https://github.com/gleam-lang/gleam)
- [Paquet Gleam HTML](https://github.com/gleam-lang/gleam_html)