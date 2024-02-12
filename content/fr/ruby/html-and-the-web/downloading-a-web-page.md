---
title:                "Téléchargement d'une page web"
aliases:
- /fr/ruby/downloading-a-web-page/
date:                  2024-01-20T17:44:32.863658-07:00
model:                 gpt-4-1106-preview
simple_title:         "Téléchargement d'une page web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Télécharger une page web, c’est récupérer son contenu via le réseau. Les programmeurs font ça pour analyser des données, tester leur disponibilité, ou automatiser des interactions.

## How to:
En Ruby, on peut utiliser `net/http` pour télécharger une page web. Simple et direct. Voici un exemple:

```Ruby
require 'net/http'
require 'uri'

url = URI.parse('https://www.example.com')
response = Net::HTTP.get_response(url)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

Et le résultat:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive
Télécharger une page web n'est pas un concept nouveau. Depuis les premiers jours d'Internet, les programmes ont récupéré le contenu des pages pour divers usages comme le web scraping ou la surveillance de contenu.

`net/http` est le joyau standard de Ruby pour HTTP. Il est suffisant pour des tâches basiques, mais il y a des alternatives comme `open-uri` pour une interface simplifiée ou des gemmes externes comme `HTTParty` et `RestClient` qui offrent plus de fonctionnalités.

Quand on implémente le téléchargement d’une page, il faut gérer les redirections, les timeouts et les erreurs de réseau. Aussi, respecter le `robots.txt` de sites est crucial si on fait du scraping.

## See Also
Pour creuser plus:
- Ruby Doc pour Net::HTTP: [ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- Documentation pour HTTParty: [github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- Documentation pour RestClient: [github.com/rest-client/rest-client](https://github.com/rest-client/rest-client)
