---
title:                "Analyse de html"
html_title:           "Clojure: Analyse de html"
simple_title:         "Analyse de html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/parsing-html.md"
---

{{< edit_this_page >}}

# Qu'est-ce que le parsing HTML et pourquoi les programmeurs le font-ils?

Le parsing HTML est le processus de lecture et d'analyse du code HTML pour en extraire des données structurées. Les programmeurs le font pour automatiser la récupération d'informations à partir de pages web, par exemple pour créer des applications de data mining ou de scraping.

# Comment faire:

```Clojure
(:require [net.cgrand.enlive-html :as html])
(html/html-snippet "<div class="example">Hello, world!</div>") ;; parse le code HTML et retourne un arbre de données
(html/select (html/html-resource "https://www.example.com") [:h1]) ;; récupère tous les éléments h1 du site web
```

# Plongée en profondeur:

Le parsing HTML est un outil couramment utilisé en programmation web pour extraire des données à partir de sites web. Il est également utilisé pour valider la structure et la conformité du code HTML. De plus, il existe des alternatives au parsing telles que l'utilisation d'API ou le web scraping manuel, mais le parsing est souvent plus rapide et plus efficace. La bibliothèque Enlive est une implémentation populaire de parsing HTML en Clojure, avec une syntaxe concise et facile à utiliser.

# Voir aussi:

- [Documentation Enlive] (https://github.com/cgrand/enlive/wiki)
- [Articles et tutoriels sur le parsing HTML en Clojure] (https://www.clojure.at/blog/tag/parsing.html)
- [Autres bibliothèques de parsing HTML en Clojure] (https://cljdoc.org/search?q=html)