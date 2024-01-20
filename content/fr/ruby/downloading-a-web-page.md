---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi et Pourquoi ?
Télécharger une page web est l'acte de récupérer et de sauvegarder le contenu d'un site web localement sur votre ordinateur. Les programmeurs le font souvent pour analyser les données, tester la disponibilité d'un site, surveiller les changements, et plus encore.

## Comment faire :
Voici comment télécharger une page Web en Ruby :

```Ruby
require 'open-uri'

open('https://www.google.com') do |f|
  File.open('web_page.html', 'w') do |file|
    file.puts(f.read)
  end
end
```
Vérifiez le fichier 'web_page.html' généré dans votre répertoire de travail. Vous y verrez la page d'accueil de Google.

## Plongée en profondeur :
Historiquement, la mécanique du web a été basée sur le protocole HTTP, dont une des fonctions principales est de permettre le téléchargement de pages web. Ruby, grâce à sa bibibliothèque standard 'open-uri', facilite remarquablement cette tâche.

En alternative, vous pouvez aussi utiliser des gemmes comme Nokogiri pour analyser le contenu HTML, ou Typhoeus pour gérer les requêtes HTTP de manière plus détaillée.

Une chose à noter : 'open-uri' dans Ruby suit les redirections par défaut. Donc, si le lien que vous avez donné redirige vers une autre URL, open-uri la suivra et téléchargera le contenu de la destination finale.

## Voir aussi:
- La documentation officielle Ruby sur 'open-uri' : https://ruby-doc.org/stdlib-2.7.0/libdoc/open-uri/rdoc/OpenURI.html
- Une introduction à Nokogiri : https://www.nokogiri.org/tutorials/getting_started.html
- Documentation de Typhoeus : https://github.com/typhoeus/typhoeus