---
title:                "Escribiendo en el error estándar"
date:                  2024-02-03T19:32:48.059339-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo en el error estándar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir en el error estándar (stderr) se trata de dirigir mensajes de error y diagnósticos al flujo stderr, separado de la salida estándar (stdout). Los programadores hacen esto para diferenciar la salida regular del programa de los mensajes de error, permitiendo una depuración y registro más efectivos.

## Cómo hacerlo:
En Clojure, puedes escribir en stderr utilizando el flujo `*err*`. Aquí tienes un ejemplo básico:

```clojure
(.write *err* "Este es un mensaje de error.\n")
```

Nota que después de escribir un mensaje, debes vaciar el flujo para asegurar que el mensaje se muestre inmediatamente:

```clojure
(flush)
```

Ejemplo de salida a stderr:
```
Este es un mensaje de error.
```

Si estás manejando excepciones, es posible que desees imprimir rastreos de pila en stderr. Usa `printStackTrace` para esto:

```clojure
(try
  ;; Código que podría lanzar una excepción
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

Para un registro de errores más estructurado, bibliotecas de terceros como `timbre` pueden configurarse para registrar en stderr. Aquí tienes una configuración y uso básicos:

Primero, añade `timbre` a tus dependencias. Luego configúralo para usar stderr:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; Deshabilita el registro en stdout
(timbre/set-config! [:appenders :spit :enabled?] false) ;; Deshabilita el registro en archivos
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; Habilita stderr para errores

(timbre/error "Ocurrió un error mientras se procesaba su solicitud.")
```

Esto dirigirá los mensajes de nivel de error a stderr, haciéndolos distintos de la salida estándar de la aplicación.
