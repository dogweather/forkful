---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:33:25.915221-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"

category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Le parsing HTML, c'est lire et transformer le HTML en informations exploitables. Les programmeurs le font pour récupérer des données, automatiser des interactions avec des sites web, ou import/export de contenu.

## How to: (Comment faire :)
```Ruby
require 'nokogiri'
require 'open-uri'

# Récupérer le HTML depuis un site web
html_content = URI.open('http://example.com').read

# Parser ce contenu avec Nokogiri
document = Nokogiri::HTML(html_content)

# Rechercher des éléments spécifiques avec CSS selectors
headings = document.css('h1').map(&:text)

puts headings
# Exemple de sortie :
# ["Welcome to example.com"]

```

## Deep Dive (Plongée en profondeur)
Historiquement, le parsing HTML était bordélique. Conçu pour être tolérant aux erreurs, le HTML peut être un cauchemar à analyser précisément. Nokogiri et d'autres bibliothèques fournissent une analyse robuste et indulgente. Qui plus est, elles gèrent les subtilités de l'encodage et la sécurisation des appels réseau.

On pourrait aussi utiliser d'autres gemmes comme Oga ou des outils intégrés comme Regexp mais chaque méthode a ses avantages et inconvénients. Nokogiri, par exemple, mise sur l'équilibre entre la facilité d'utilisation et la performance grâce à sa construction sur libxml2.

Et le choix de la bibliothèque peut dépendre des besoins : extraction rapide de données, sécurité, compatibilité, ou la consommation de ressources.

## See Also (Voir aussi)
- Nokogiri documentation: [https://nokogiri.org/](https://nokogiri.org/)
- 'open-uri' Ruby Standard Library: [https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html)
- Ruby Regexp documentation: [https://ruby-doc.org/core-3.0.0/Regexp.html](https://ruby-doc.org/core-3.0.0/Regexp.html)
