---
date: 2024-01-26 04:20:30.292412-07:00
description: "Trabajar con TOML significa que est\xE1s manejando datos en un formato\
  \ minimalista \"Tom's Obvious, Minimal Language\" (Lenguaje Minimalista y Obvio\
  \ de Tom),\u2026"
lastmod: 2024-02-19 22:05:17.271186
model: gpt-4-0125-preview
summary: "Trabajar con TOML significa que est\xE1s manejando datos en un formato minimalista\
  \ \"Tom's Obvious, Minimal Language\" (Lenguaje Minimalista y Obvio de Tom),\u2026"
title: Trabajando con TOML
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con TOML significa que estás manejando datos en un formato minimalista "Tom's Obvious, Minimal Language" (Lenguaje Minimalista y Obvio de Tom), popular para archivos de configuración debido a su fácil legibilidad. Los programadores lo utilizan para la gestión de configuraciones sencilla que funciona directamente y con una sintaxis amigable para los humanos.

## Cómo hacerlo:
Para trabajar con TOML en Clojure, necesitas una biblioteca como `clj-toml`. Primero, agrégala a tu `deps.edn`:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Luego, analiza algún TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'Ejemplo TOML'")

(def parsed-config (toml/parse-string config-str))

;; Obtén el título del TOML analizado
(println (:title parsed-config)) ;; Salida: Ejemplo TOML
```

Para generar TOML:

```clojure
(def data {:title "Ejemplo TOML"})

(println (toml/generate-string data))
;; Salida: title = "Ejemplo TOML"
```

## Análisis profundo
TOML fue creado alrededor de 2013 por Tom Preston-Werner, co-fundador de GitHub, como una alternativa más sencilla a YAML y JSON para archivos de configuración. Busca la claridad e intenta ser una especificación que los humanos puedan leer sin herramientas adicionales.

Mientras que JSON se usa a menudo para APIs y aplicaciones web, y YAML puede complicarse con referencias y capacidades de script, TOML se destaca con un enfoque en estructuras simples basadas en tablas. Esta simplicidad lo hace especialmente popular en la comunidad Rust y otros entornos de lenguajes modernos.

Clojure, con su enfoque en la simplicidad y practicidad, se empareja bien con TOML para configuración. `clj-toml` u otras bibliotecas alternativas cierran la brecha. Traducen los datos estáticos de TOML al mundo dinámico y funcional de Clojure.

## Ver también
- Repositorio de GitHub de TOML: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` en Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Documentación de Clojure: [clojure.org](https://clojure.org/guides/getting_started)
- Introducción a `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
