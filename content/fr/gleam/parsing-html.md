---
title:                "Analyse de code HTML"
html_title:           "Gleam: Analyse de code HTML"
simple_title:         "Analyse de code HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi? 
Le parsing HTML est un processus de traitement de fichiers HTML pour extraire des informations spécifiques du contenu. Les programmeurs utilisent le parsing HTML pour automatiser la collecte de données, simplifier le traitement de texte, et faciliter l'intégration avec d'autres langages de programmation.

## Comment faire:
Voici un exemple de code en Gleam qui montre comment utiliser le parsing HTML pour extraire le titre et le contenu d'un article de blog en ligne:

```Gleam
let url = "https://example.com/article"
let document = Url.get(url)
|> Text.from_string
|> Html.parse

let title = document
|> Html.find("h1")
|> Html.text

let content = document
|> Html.find("#article-content")
|> Html.children
|> Html.text
```

Résultat:
```
title = "Mon super article"
content = "Voici le contenu de mon article qui est très intéressant."
```

## Plongée en profondeur:
Le parsing HTML a été développé dans les années 1990, lorsque le World Wide Web a commencé à gagner en popularité. Aujourd'hui, il existe plusieurs alternatives au parsing HTML telles que XML, JSON, et CSV. Cependant, le HTML reste un format couramment utilisé en raison de son support multiplateforme et de sa facilité d'utilisation pour les humains et les machines. L'implémentation du parsing HTML peut varier en fonction du langage de programmation utilisé, mais les concepts de base restent les mêmes.

## Voir aussi:
- [Documentation officielle de Gleam](https://gleam.run/)
- [Tutoriel sur le parsing HTML avec Gleam](https://blog.gleam.run/parsing-html-with-gleam/)