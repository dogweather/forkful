---
title:                "Uso de expresiones regulares"
html_title:           "Clojure: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Usar expresiones regulares es una forma de buscar, encontrar y manipular patrones de texto en una cadena de caracteres. Los programadores las utilizan para realizar tareas como validar datos de entrada y transformar cadenas de texto en un formato específico. 

## ¿Cómo hacerlo?

Utiliza el operador `re-find` y el operador `re-matches` para encontrar coincidencias en una cadena de texto según un patrón establecido. 

```Clojure 
(re-find #"hola" "hola mundo") ; => "hola"
(re-find #"adiós" "hola mundo") ; => nil 

(re-matches #"he(l+)o" "hello") ; => ["hello" "ll"]
```

Puedes utilizar el operador `cond` para realizar una acción según una coincidencia encontrada. 

```Clojure 
(cond
  (re-find #"perro" "Me gusta mi perro") "¡Me encantan los perros!"
  (re-find #"gato" "Me gusta mi perro") "¿Gatos? ¡No, gracias!"
  :else "No encontré ningún animal")
; => "¡Me encantan los perros!" 
```

## Profundizando

Las expresiones regulares fueron desarrolladas por el matemático Stephen Cole Kleene en la década de 1950 y popularizadas por el informático Ken Thompson en la década de 1960. Son una herramienta poderosa pero pueden resultar difíciles de leer y mantener en patrones complejos. En lugar de utilizar expresiones regulares, también puedes considerar el uso de funciones de cadena de texto como `replace` o `split` para lograr resultados similares. 

## Ver también

Para obtener más información sobre el uso de expresiones regulares en Clojure, consulta la documentación oficial en [clojure.github.io](https://clojure.github.io/clojure/)