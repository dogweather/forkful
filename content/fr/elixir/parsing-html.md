---
title:                "Elixir: Analyse de HTML"
simple_title:         "Analyse de HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Le parsing HTML est une compétence essentielle pour tout programmeur Elixir. Cela permet de traiter les données structurées dans des pages web, rendant ainsi possible l'extraction et la manipulation de ces informations. Si vous êtes un développeur web ou que vous travaillez avec des données issues du web, le parsing HTML est une compétence incontournable à maîtriser.

## Comment faire

Pour commencer, nous avons besoin d'une bibliothèque Elixir appelée "Floki" qui facilite le parsing HTML en utilisant une syntaxe similaire à CSS. Voici un exemple de document HTML que nous allons utiliser pour illustrer notre code :

```Elixir
<!DOCTYPE html>
<html>
  <head>
    <title>Blog Elixir</title>
  </head>
  <body>
    <h1>Bienvenue sur mon blog Elixir !</h1>
    <ul>
      <li>Article 1</li>
      <li>Article 2</li>
      <li>Article 3</li>
    </ul>
  </body>
</html>
```

Tout d'abord, nous devons charger le document HTML dans notre code en utilisant la fonction "html_to_document" de Floki :

```Elixir
html = """
  <!DOCTYPE html>
  <html>
    <head>
      <title>Blog Elixir</title>
    </head>
    <body>
      <h1>Bienvenue sur mon blog Elixir !</h1>
      <ul>
        <li>Article 1</li>
        <li>Article 2</li>
        <li>Article 3</li>
      </ul>
    </body>
  </html>
"""

doc = Floki.html_to_document(html)
```

Nous pouvons ensuite utiliser la fonction "find" pour rechercher des éléments spécifiques dans notre document HTML. Par exemple, si nous voulons extraire le titre de notre blog, nous pouvons utiliser la syntaxe CSS "h1" pour trouver l'élément "h1" et récupérer son contenu à l'aide de la fonction "text" :

```Elixir
title = Floki.find(doc, "h1") 
|> Floki.text()
```

Nous pouvons également utiliser la fonction "every" pour trouver tous les éléments correspondant à notre sélecteur CSS. Par exemple, pour récupérer tous les articles de notre blog, nous pouvons utiliser la syntaxe CSS "li" pour trouver tous les éléments "li" et utiliser la fonction "text" pour récupérer leur contenu :

```Elixir
articles = Floki.find(doc, "li") 
|> Floki.every(&Floki.text/1)
```

Et voilà, nous avons maintenant le titre de notre blog et tous les articles dans une liste prêts à être utilisés dans notre code !

## Plongée en profondeur

Maintenant que vous avez une compréhension de base du parsing HTML en Elixir, vous pouvez explorer davantage en utilisant les différentes fonctions proposées par la bibliothèque Floki. Vous pouvez également apprendre à utiliser des sélecteurs CSS plus avancés pour cibler des éléments spécifiques dans votre document HTML.

Il est également important de noter que Floki prend en charge les expressions XPath (un autre langage de requête pour les documents XML), ce qui vous permet d'interagir avec des documents HTML et XML de manière plus avancée et plus flexible.

## Voir aussi

- Documentation officielle de Floki : https://hexdocs.pm/floki/Floki.html
- Tutoriel sur le parsing HTML en Elixir : https://www.jungledisk.com/blog/2016/06/22/elixir-parsing-html-using-css-selectors/
- Bibliothèques Elixir pour le web scraping : http://elixirresources.s3-website-us-east-1.amazonaws.com/scraping.html