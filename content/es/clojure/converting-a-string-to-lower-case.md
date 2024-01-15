---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Clojure: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas?

Hay varias razones por las que puedes querer convertir una cadena a minúsculas en Clojure. Por ejemplo, puede ser necesario para comparar cadenas de manera más precisa, ya que convertirlas a minúsculas eliminará las diferencias de capitalización. También puede ser necesario para cumplir con ciertas convenciones de estilo en un proyecto de programación.

## Cómo hacerlo

Es muy fácil convertir una cadena a minúsculas en Clojure. Simplemente tienes que usar la función ```clojure (lower-case)``` con la cadena que quieres convertir como argumento. Aquí hay un ejemplo:

```Clojure
 (def my-str "Hola Mundo")
 (lower-case my-str)
```
*Salida: "hola mundo"*

Como puedes ver, todas las letras en la cadena se han convertido a minúsculas. Además, también puedes aplicar esta función directamente a una cadena literal, como en el siguiente ejemplo:

```Clojure
 (lower-case "BONJOUR")
```
*Salida: "bonjour"*

## Profundizando en la conversión de cadenas a minúsculas

Cuando usas la función ```lower-case``` en una cadena, se aplican las reglas de conversión de minúsculas de Unicode. Estas reglas se basan en el estándar internacional para codificar caracteres. Por lo tanto, la conversión a minúsculas puede variar según el idioma y los caracteres específicos de la cadena. Además, debes tener en cuenta que cualquier caracter que ya esté en minúscula no se verá afectado por esta función.

## Consulta también

- [Documentación oficial de Clojure sobre lower-case](https://clojuredocs.org/clojure.core/lower-case)
- [¿Cómo convertir una cadena a mayúsculas en Clojure?](https://github.com/your-username/your-project-name/blob/master/README.md) *nota: reemplaza "your-username" y "your-project-name" con tu información correspondiente*