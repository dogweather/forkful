---
date: 2024-01-20 17:59:25.263233-07:00
description: "How to: (Comment faire :) Pour envoyer une requ\xEAte HTTP en C++, vous\
  \ pouvez utiliser la biblioth\xE8que `cpr`. Voici comment ."
lastmod: '2024-04-05T21:53:59.587015-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) Pour envoyer une requ\xEAte HTTP en C++, vous pouvez utiliser\
  \ la biblioth\xE8que `cpr`."
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

## How to: (Comment faire :)
Pour envoyer une requête HTTP en C++, vous pouvez utiliser la bibliothèque `cpr`. Voici comment :

```C++
#include <cpr/cpr.h>
#include <iostream>

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    std::cout << "Status code: " << r.status_code << std::endl;
    std::cout << "Response body: " << r.text << std::endl;
}
```

Sortie :
```
Status code: 200
Response body: {
  ...
  "url": "http://httpbin.org/get"
}
```

## Deep Dive (Plongée en profondeur)
Dans le passé, les programmeurs utilisaient `libcurl` en C++ pour les requêtes HTTP, mais elle est plus bas niveau et complexe. `cpr` est une bibliothèque moderne qui offre une interface plus simple, inspirée de Python `requests`. L'implémentation suit le paradigme RAII (Resource Acquisition Is Initialization), simplifiant la gestion des ressources.

## See Also (Voir aussi)
- Documentation `cpr` : https://whoshuu.github.io/cpr/
- Documentation `libcurl` : https://curl.haxx.se/libcurl/c/
- Tutoriel `libcurl` en C++ : https://curl.haxx.se/libcurl/c/libcurl-tutorial.html
- Guide des requêtes HTTP avec `Boost.Beast` : https://www.boost.org/doc/libs/1_75_0/libs/beast/doc/html/index.html
