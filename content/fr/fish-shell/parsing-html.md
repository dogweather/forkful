---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:31:19.107398-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? | Quoi & Pourquoi ?
Parsing HTML, c'est décortiquer le contenu d'une page web pour le comprendre et l'utiliser. Les programmeurs le font pour récupérer des données, automatiser des tâches web ou tester des sites.

## How to | Comment faire :
```Fish Shell
# Installer pup, un parseur HTML en ligne de commande
brew install pup

# Utiliser pup pour extraire des titres depuis un fichier HTML
cat index.html | pup 'h1 text{}'
```
Sortie de l'exemple :
```
Mon Super Site Web
```

```Fish Shell
# Récupérer des liens d'une page web avec wget et les parser avec pup
wget -O - https://exemple.com | pup 'a attr{href}'
```
Sortie de l'exemple :
```
/page1
/contact
/about.html
```

## Deep Dive | Exploration approfondie
Le parsing HTML a commencé avec le développement du web. Aujourd'hui, il y a beaucoup d'outils comme Beautiful Soup pour Python, mais poussant la simplicité, `pup` s'aligne bien avec le minimalisme de Fish. Fish Shell, sorti en 2005, n'est pas conçu pour le parsing HTML à l'origine, mais son pipelining efficace le rend utile pour ce genre de tâches scriptées.

Des alternatives incluent des bibliothèques de parsing côté serveur ou des API qui peuvent traiter le HTML avant qu'il atteigne le navigateur. Pour Fish, un bon compromis entre performance et simplicité est crucial, alors que les méthodes de parsing peuvent varier selon les besoins spécifiques et la complexité du HTML.

## See Also | Voir également
- Documentation de `pup` : [https://github.com/ericchiang/pup](https://github.com/ericchiang/pup)
- Guide de Fish Shell : [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Beautiful Soup Documentation : [https://www.crummy.com/software/BeautifulSoup/bs4/doc/](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
