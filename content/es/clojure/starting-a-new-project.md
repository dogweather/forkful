---
title:                "Comenzando un nuevo proyecto"
html_title:           "Clojure: Comenzando un nuevo proyecto"
simple_title:         "Comenzando un nuevo proyecto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Cuando eres un programador, siempre estás buscando formas de mejorar tus habilidades y aprender nuevas tecnologías. Una forma de hacerlo es empezar un nuevo proyecto. Esto te brinda la oportunidad de poner en práctica tus conocimientos y explorar ideas nuevas.

## Cómo:

```Clojure
(defn nuevo-proyecto [nombre objetivo]
  (format "¡Felicidades, has iniciado un nuevo proyecto llamado %s con el objetivo de %s!" 
  nombre objetivo))

(defn proyecto [nombre]
  (println (format "Bienvenido al proyecto %s" nombre)))
```

Salida:

```Clojure
(nuevo-proyecto "Analizador de texto" "identificar patrones en documentos")
;; ¡Felicidades, has iniciado un nuevo proyecto llamado Analizador de texto con el objetivo de identificar patrones en documentos!

(proyecto "Analizador de texto")
;; Bienvenido al proyecto Analizador de texto
```

## Buceo profundo:

Iniciar un nuevo proyecto en Clojure puede parecer intimidante, pero la comunidad es muy solidaria y hay muchos recursos disponibles para ayudarte. Antes de comenzar, asegúrate de tener una comprensión sólida de Clojure y su sintaxis. Alternativamente, también puedes considerar utilizar plantillas de proyectos existentes como Leiningen o Clojure CLI para facilitar el proceso.

## Ver también:

- [Documentación oficial de Clojure] (https://clojure.org/)
- [Leiningen] (https://leiningen.org/)
- [Clojure CLI] (https://clojure.org/guides/deps_and_cli)