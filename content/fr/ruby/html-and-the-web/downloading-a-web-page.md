---
aliases:
- /fr/ruby/downloading-a-web-page/
date: 2024-01-20 17:44:32.863658-07:00
description: "T\xE9l\xE9charger une page web, c\u2019est r\xE9cup\xE9rer son contenu\
  \ via le r\xE9seau. Les programmeurs font \xE7a pour analyser des donn\xE9es, tester\
  \ leur disponibilit\xE9, ou\u2026"
lastmod: 2024-02-18 23:09:09.405988
model: gpt-4-1106-preview
summary: "T\xE9l\xE9charger une page web, c\u2019est r\xE9cup\xE9rer son contenu via\
  \ le r\xE9seau. Les programmeurs font \xE7a pour analyser des donn\xE9es, tester\
  \ leur disponibilit\xE9, ou\u2026"
title: "T\xE9l\xE9chargement d'une page web"
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
