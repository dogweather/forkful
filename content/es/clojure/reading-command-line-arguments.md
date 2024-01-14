---
title:    "Clojure: Leyendo argumentos de línea de comando"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##

Por qué: Es importante entender cómo leer argumentos de línea de comando para poder construir aplicaciones de Clojure más interactivas y versátiles.

Cómo hacerlo: Hay varias maneras de leer argumentos de línea de comando en Clojure, pero la forma más común es usando la función "clojure.main/command-line". Aquí hay un ejemplo de cómo utilizarlo:

```Clojure
(defn saludar [nombre]
  (println "¡Hola," nombre "!"))

(defn -main [& args]
  (when-let [nombre (first args)]
    (saludar nombre)))

```

Si ejecutas este programa con el argumento "Marta", deberías obtener el siguiente resultado:

```
¡Hola, Marta!
```

Profundizando: La función "clojure.main/command-line" toma dos argumentos: una función y una secuencia de argumentos. La función que le pases será ejecutada con los argumentos de línea de comando como parámetros. También puedes usar la macro "clojure.main/with-command-line" para manejar los argumentos de manera más flexible.

De manera similar, puedes leer argumentos de línea de comando usando la biblioteca "tools.cli" de Clojure. Aquí hay un ejemplo de cómo hacerlo:

```Clojure
(ns mi-app.core
  (:require [clojure.tools.cli :refer [parse-opts]]))

(defn saludar
  [{:keys [nombre]}]
  (println "¡Hola," nombre "!"))

(defn -main [& args]
  (let [[opts _] (parse-opts args [["-n" "--nombre NOMBRE" "Tu nombre"]] {:summary "Saluda a alguien."})]
    (when-let [nombre (:nombre opts)]
      (saludar {:nombre nombre})))
```

Si ejecutas este programa con el argumento "--nombre Marta", deberías obtener el mismo resultado que antes.

Ver también: Para una explicación más detallada sobre cómo leer argumentos de línea de comando en Clojure, puedes consultar la documentación oficial de Clojure sobre "clojure.main/command-line" y la biblioteca "tools.cli". También puedes explorar otras bibliotecas disponibles para manejar argumentos de línea de comando en Clojure. ¡Inténtalo y haz que tus aplicaciones sean más interactivas y versátiles!