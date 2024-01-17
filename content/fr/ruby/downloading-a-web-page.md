---
title:                "Le téléchargement d'une page web"
html_title:           "Ruby: Le téléchargement d'une page web"
simple_title:         "Le téléchargement d'une page web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Télécharger une page web en utilisant Ruby

## Qu'est-ce que c'est et pourquoi le font-ils?
Télécharger une page web signifie récupérer son contenu à partir d'Internet et le stocker dans un fichier. Les programmeurs font cela pour accéder rapidement aux informations en ligne et les utiliser dans leurs propres logiciels et projets.

## Comment faire:
Pour télécharger une page web en utilisant Ruby, vous pouvez utiliser la gem 'open-uri' et la méthode 'open'. Voici un exemple de code qui télécharge une page web et en affiche le contenu:

```
require 'open-uri'

content = open("https://www.google.com").read

puts content
```

Cela va récupérer le contenu de la page d'accueil de Google et l'afficher dans la console. Vous pouvez également spécifier le fichier dans lequel vous souhaitez enregistrer le contenu en utilisant la méthode 'open' avec un bloc, comme ceci:

```
open("https://www.google.com") do |f|
    File.open("google.html", "w") do |file|
        file.puts f.read
    end
end
```

Cela va créer un fichier nommé 'google.html' dans lequel le contenu de la page sera enregistré. 

## Plongée en profondeur:
La possibilité de télécharger des pages web a été ajoutée dans Ruby 1.8 avec l'introduction de la gem 'open-uri'. Il existe également des alternatives pour télécharger des pages web telles que la gem 'httparty' ou en utilisant des bibliothèques externes telles que 'cURL'.

L'implémentation de la méthode 'open' utilise la bibliothèque standard 'net/http' pour effectuer des requêtes HTTP. Vous pouvez également spécifier des options telles que les en-têtes de requête et les paramètres de redirection pour personnaliser votre requête.

## Voir aussi:
- [La documentation officielle de Ruby pour la gem 'open-uri'](http://ruby-doc.org/stdlib-2.7.0/libdoc/open-uri/rdoc/OpenURI.html)
- [La documentation officielle de Ruby pour la bibliothèque 'net/http'](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [La gem 'httparty' pour télécharger des pages web en utilisant Ruby](https://github.com/jnunemaker/httparty)
- [La bibliothèque 'cURL' pour télécharger des pages web en utilisant Ruby](https://curl.haxx.se/libcurl/r/ruby.html)