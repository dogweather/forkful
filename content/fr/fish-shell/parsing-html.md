---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Parse le HTML signifie examiner et extraire des informations évidentes et cachées d'un document HTML. Les programmeurs le font pour manipuler, extraire ou analyser le contenu d'un site web.

## Comment le faire :
Avec Fish Shell, vous pouvez utiliser des outils comme pup ou html-xml-utils pour analyser le HTML. 
Voici un exemple avec pup :

```Fish Shell
echo "<html><body><p>Hello world</p></body></html>" | pup 'p text{}'
```

Sortie:
```Fish Shell
Hello world
```
Cet exemple vous montre comment extraire le texte à l'intérieur d'une balise <p>.

## Approfondissement 
Historiquement, Parse HTML est une tâche difficile en raison des nombreuses irrégularités et spécifications de balisage des langages HTML. Les alternatives à pup et html-xml-utils incluent des parseurs plus volumineux et robustes comme BeautifulSoup en Python ou Nokogiri en Ruby. L'implémentation de ces outils examine généralement chaque balise et attribut, en les structurant dans une forme facilement navigable et analysable.

## Voir aussi
Pour plus de détails, consultez les pages de manuel de [pup](https://github.com/EricChiang/pup) et [html-xml-utils](https://www.w3.org/Tools/HTML-XML-utils/). 
Pour des alternatives, consultez [BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) et [Nokogiri](https://nokogiri.org/tutorials/parsing_an_html_xml_document.html).