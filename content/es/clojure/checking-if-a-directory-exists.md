---
title:    "Clojure: Verificando si existe un directorio"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
Hay varias razones por las cuales un programador podría necesitar verificar si un directorio existe en Clojure. Por ejemplo, puede ser necesario para determinar si un directorio está disponible para guardar archivos descargados o para buscar dependencias externas.

## Cómo hacerlo
Para verificar si un directorio existe en Clojure, podemos usar la función `file-seq`, que tomará un objeto de tipo `Java.io.File` como argumento. Aquí hay un ejemplo de cómo usarlo:

```Clojure
(defn existe-directorio? [ruta-directorio]
  (if (some? (file-seq (java.io.File. ruta-directorio)))
    true
    false))

(def resultado (existe-directorio? "ruta de ejemplo"))
(println resultado)
```

Al ejecutar este código, si el directorio existe en la ruta proporcionada, obtendremos un resultado de `true`. Por el contrario, si el directorio no existe, el resultado será `false`.
¡Eso es todo! Con unas pocas líneas de código, podemos verificar fácilmente si un directorio existe en Clojure.

## Profundizando
Si deseas profundizar más en el tema, puedes explorar otras funciones que pueden ser útiles al trabajar con directorios en Clojure, como `file`, `make-parents`, `list-files` y `list-directories`. También puedes revisar la documentación oficial de Clojure para obtener más información sobre cómo trabajar con archivos y directorios.

## Ver también
- [Documentación oficial de Clojure para manejo de archivos (en inglés)](https://clojure.org/reference/io)
- [Ejemplo de codificación para verificar si un directorio existe en Clojure (en inglés)](https://gist.github.com/aphyr/3824575)
- [Tutoriales de Clojure (en español)](http://clojurespanol.cl/tutoriales/)