---
aliases:
- /fr/clojure/downloading-a-web-page/
date: 2024-01-20 17:43:30.126068-07:00
description: "T\xE9l\xE9charger une page web, c'est r\xE9cup\xE9rer son contenu via\
  \ Internet. Les programmeurs le font pour analyser des donn\xE9es, automatiser des\
  \ t\xE2ches ou agr\xE9ger\u2026"
lastmod: 2024-02-18 23:09:08.385426
model: gpt-4-1106-preview
summary: "T\xE9l\xE9charger une page web, c'est r\xE9cup\xE9rer son contenu via Internet.\
  \ Les programmeurs le font pour analyser des donn\xE9es, automatiser des t\xE2ches\
  \ ou agr\xE9ger\u2026"
title: "T\xE9l\xE9chargement d'une page web"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Télécharger une page web, c'est récupérer son contenu via Internet. Les programmeurs le font pour analyser des données, automatiser des tâches ou agréger du contenu.

## Comment faire :
```Clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (let [response (client/get url)]
    (:body response)))

;; Utilisation
(println (download-page "https://www.example.com"))
```

Sortie d'échantillon :
```
<!doctype html>
<html>
<head>
    <title>Exemple de Titre</title>
...
```

## Exploration:
Historiquement, télécharger des pages web était plus laborieux, souvent fait avec de grosses bibliothèques Java ou par des commandes comme `wget`. Maintenant, en Clojure, des libs comme `clj-http` simplifient la tâche. Alternativement, on pourrait utiliser `http-kit` ou `aleph` pour des fonctionnalités asynchrones. Quand on télécharge une page web, il faut s’occuper de la gestion des erreurs réseau et de l’encodage - clj-http gère ça pour vous.

## Voir aussi :
- Documentation de `clj-http` : [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Documentation de `http-kit` : [http://www.http-kit.org/](http://www.http-kit.org/)
- Documentation de `aleph` : [https://github.com/clj-commons/aleph](https://github.com/clj-commons/aleph)
