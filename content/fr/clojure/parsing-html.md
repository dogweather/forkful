---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi?

L'analyse HTML, en somme, signifie la transformation des données HTML en une structure que votre programme peut utiliser. Les programmeurs le font pour récupérer des données d'un document HTML.

## Comment faire:

```clojure
(use 'enlive.core)
(def page (html-resource (java.net.URL. "http://example.com")))

(defn parse-data [node]
  {:title (-> node (select [:#title]) text)})
```

Envoie:
```clojure
{:title "Example Domain"}
```

La bibliothèque Clojure `enlive` se charge tout simplement de la tâche d'analyse. Ici, nous obtenons le titre de la page à l'aide de la fonction `select`.

## Plongée profonde:

L'analyse HTML a commencé dans le besoin de récupérer et manipuler les informations disponibles sur le web. Aujourd'hui, de nombreux outils comme BeautifulSoup (Python), Nokogiri (Ruby) et maintenant `enlive` (Clojure) existent à cette fin.

En Clojure, en live offre un moyen idiomatique d'analyser HTML. Cependant, il n'est pas le seul. Des options comme `jsoup` et `hickory` sont disponibles si `enlive` ne répond pas à vos besoins.

En termes de mise en œuvre, `enlive` utilise des documents basés sur les nœuds pour représenter et manipuler le HTML. Cela signifie que nous pouvons transmettre des parties du document à des fonctions pour une manipulation plus fine.

## Voir aussi:

Enlive GitHub - https://github.com/cgrand/enlive

Document Clojure on parsing HTML with Enlive - https://clojuredocs.org/clojure.xml/parse

Jsoup: Java HTML Parser - https://jsoup.org/

Hickory, a Clojure library for parsing HTML - https://github.com/davidsantiago/hickory

N'oubliez pas de tester et d'expérimenter avec différentes bibliothèques pour voir laquelle conviendra le mieux à votre projet!