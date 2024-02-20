---
date: 2024-01-20 18:03:14.201533-07:00
description: "Iniciar un nuevo proyecto es b\xE1sicamente establecer el terreno para\
  \ tu futuro c\xF3digo. Los programadores lo hacen para comenzar con una base estructurada\
  \ y\u2026"
lastmod: 2024-02-19 22:05:17.248557
model: gpt-4-1106-preview
summary: "Iniciar un nuevo proyecto es b\xE1sicamente establecer el terreno para tu\
  \ futuro c\xF3digo. Los programadores lo hacen para comenzar con una base estructurada\
  \ y\u2026"
title: Iniciando un nuevo proyecto
---

{{< edit_this_page >}}

## Qué y por qué?
Iniciar un nuevo proyecto es básicamente establecer el terreno para tu futuro código. Los programadores lo hacen para comenzar con una base estructurada y ordenada, lo que facilita el desarrollo y el mantenimiento a largo plazo.

## Cómo hacerlo:
Para comenzar un nuevo proyecto en Clojure, puedes usar Leiningen o la CLI (Command Line Interface) de Clojure. Aquí te muestro cómo hacerlo con Leiningen:

```Clojure
;; Instala Leiningen siguiendo las instrucciones en https://leiningen.org/
;; Luego, crea un nuevo proyecto con este comando:
lein new app nombre-de-tu-proyecto

;; Esto generará la siguiente estructura de directorios:
; nombre-de-tu-proyecto/
;   README.md
;   doc/
;     intro.md
;   project.clj
;   resources/
;   src/
;     nombre_de_tu_proyecto/
;       core.clj
;   test/
;     nombre_de_tu_proyecto/
;       core_test.clj
```

Con Clojure CLI, el proceso es similar:

```Clojure
;; Asegúrate de tener instalado Clojure CLI
;; Crear un nuevo proyecto:
clojure -M:new app nombre-de-tu-proyecto

;; Estructura similar a la creada por Leiningen
```

Después de crear tu proyecto, puedes empezar a desarrollar tu aplicación dentro del archivo `src/nombre_de_tu_proyecto/core.clj`.

## Profundizando
Históricamente, en Clojure se ha utilizado Leiningen como la herramienta estándar para manejar proyectos. Sin embargo, con la evolución del lenguaje, Clojure CLI ha ganado popularidad debido a su integración más directa con las herramientas de Clojure.

Leiningen se destaca por su simplicidad y plugins disponibles, mientras que Clojure CLI brinda una experiencia más "clojurística" y se integra bien con las herramientas de deps.edn para gestionar dependencias. Anteriormente, uno usaba `project.clj` para manejar dependencias y configuraciones, pero con Clojure CLI, ahora usamos `deps.edn`, que es más sencillo y está más alineado con la filosofía de Clojure.

Independientemente de la herramienta que elijas, ambas te ayudarán a crear un esqueleto de proyecto que sigue las convenciones comunes de Clojure, ayudándote a mantener un código limpio y organizado.

## Ver también
Para más detalles y guías, consulta los siguientes recursos:
- Leiningen: [Guía de inicio](https://leiningen.org/)
- Clojure CLI: [Referencia de herramientas CLI](https://clojure.org/guides/deps_and_cli)
- Tutorial de Clojure para principiantes de Brave Clojure: [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
