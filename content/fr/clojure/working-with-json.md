---
title:                "Travailler avec json"
html_title:           "Clojure: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-json.md"
---

{{< edit_this_page >}}

Qu'est-ce que le traitement JSON et pourquoi les programmeurs l'utilisent?

JSON, ou JavaScript Object Notation, est un format de données populaire utilisé pour stocker et échanger des informations structurées. Il est principalement utilisé dans le développement web pour transférer des données entre un serveur et un navigateur. Les programmeurs l'utilisent car il est facile à lire et à écrire, et est pris en charge par de nombreux langages de programmation.

Comment faire:

```Clojure 
;; pour convertir un objet Clojure en JSON
(clojure.data.json/write-str {:nom "John" :age 25})

// {"nom": "John", "age": 25}

;; pour convertir une chaîne JSON en objet Clojure
(clojure.data.json/read-str "{\"nom\": \"Jane\", \"age\": 30}")

// {:nom "Jane", :age 30}
```

Plongée en profondeur:

JSON a été créé en 2001 pour résoudre les problèmes liés au traitement des données dans les applications web. Avant JSON, XML était principalement utilisé, mais il était plus compliqué à lire et écrire. D'autres alternatives à JSON incluent YAML et EDN. En interne, Clojure utilise la bibliothèque Jackson pour convertir des données entre Clojure et JSON.

À voir:

- Bibliothèque de traitement JSON Jackson: https://github.com/FasterXML/jackson
- Documentation Clojure pour la bibliothèque de données JSON: https://clojure.github.io/data.json/