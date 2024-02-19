---
aliases:
- /es/clojure/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:19.641755-07:00
description: "Las expresiones regulares, una herramienta poderosa para el emparejamiento\
  \ de patrones y la manipulaci\xF3n de datos, son esenciales en tareas de\u2026"
lastmod: 2024-02-18 23:09:09.592395
model: gpt-4-0125-preview
summary: "Las expresiones regulares, una herramienta poderosa para el emparejamiento\
  \ de patrones y la manipulaci\xF3n de datos, son esenciales en tareas de\u2026"
title: Usando expresiones regulares
---

{{< edit_this_page >}}

## Qué y Por Qué?
Las expresiones regulares, una herramienta poderosa para el emparejamiento de patrones y la manipulación de datos, son esenciales en tareas de procesamiento de texto como validar entradas, buscar y reemplazar texto. Los programadores las utilizan extensamente para manejar tareas de análisis y validación de datos complejos de manera eficiente y sucinta.

## Cómo hacerlo:
Clojure, manteniéndose fiel a sus raíces en la familia Lisp, ofrece un rico conjunto de funciones que se interfazan sin problemas con las capacidades de expresiones regulares de Java. Aquí te mostramos cómo puedes aprovecharlas:

### Coincidencia Básica
Para verificar si una cadena coincide con un patrón, usa `re-matches`. Devuelve la coincidencia completa si tiene éxito o `nil` en caso contrario.

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### Buscando Patrones
Para encontrar la primera ocurrencia de un patrón, `re-find` es tu función a utilizar:

```clojure
(re-find #"\d+" "Orden 123")  ;=> "123"
```

### Grupos de Captura
Utiliza `re-find` junto con paréntesis en tu patrón para capturar grupos:

```clojure
(let [[_ área código] (re-find #"(1)?(\d{3})" "Teléfono: 123-4567")]
  (println "Código de Área:" área "Código:" código))
;; Salida: Código de Área: nil Código: 123
```

### Búsqueda Global (Encontrar Todas las Coincidencias)
Clojure no tiene una búsqueda global incorporada como algunos lenguajes. En su lugar, usa `re-seq` para obtener una secuencia perezosa de todas las coincidencias:

```clojure
(re-seq #"\d+" "id: 123, cant: 456")  ;=> ("123" "456")
```

### Dividiendo Cadenas
Para dividir una cadena basada en un patrón, usa `clojure.string/split`:

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### Reemplazando
Reemplaza partes de una cadena que coincidan con un patrón con `clojure.string/replace`:

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "AAAA")  ;=> "AAAA-04-01"
```

### Bibliotecas de Terceros
Aunque el soporte integrado de Clojure es suficiente para la mayoría de los casos, para escenarios más complejos, considera usar bibliotecas como `clojure.spec` para una validación de datos robusta y `reagent` para manipulación del DOM reactiva en aplicaciones web con enrutamiento basado en regex y validación de entrada.

```clojure
;; Ejemplo usando clojure.spec para validar un correo electrónico
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> true
```

Recuerda, mientras que las expresiones regulares son poderosas, también pueden hacer que el código sea difícil de leer y mantener. Úsalas con juicio y siempre considera funciones de manipulación de cadenas más simples cuando sea posible.
