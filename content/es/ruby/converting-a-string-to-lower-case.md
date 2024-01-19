---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Convertir una cadena a minúsculas en Ruby significa convertir todas las letras mayúsculas en dicha cadena a minúsculas. Los programadores hacen esto para normalizar los datos de entrada y hacer las comparaciones insensibles a mayúsculas y minúsculas.

## Cómo se hace:

En Ruby, se utiliza el método `downcase` para convertir una cadena a minúsculas. Aquí está el código de ejemplo y la salida de muestra:

```Ruby
texto = "Hola Mundo"
puts texto.downcase
```

Salida:

```Ruby
"hola mundo"
```

## Análisis en Profundidad:

1. **Historia**: El método `downcase` ha sido parte de Ruby desde la versión 1.8.7. Su propósito primordial es proporcionar una forma consistente de hacer comparaciones y búsquedas insensibles a la caja.

2. **Alternativas**: Si solo necesitas convertir letras específicas a minúsculas, puedes usar `tr`. Por ejemplo, `str.tr('A-Z','a-z')` convertirá todas las letras mayúsculas del alfabeto inglés a minúsculas. 

3. **Detalles de implementación**: El método `downcase` usa las tablas de mapeo de Unicode para la conversión a minúsculas. Esto asegura la precisión incluso con cadenas que contienen letras acentuadas y otros caracteres especiales.

## Consultar También:

Para aprender más acerca de las cadenas en Ruby, consulta los siguientes enlaces:

1. Documentación Oficial de Ruby para Clase String: [https://ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)

2. Tutoriales de Ruby sobre Cadenas y Métodos de Cadenas: [https://www.rubyguides.com/ruby-tutorial/ruby-string-methods/](https://www.rubyguides.com/ruby-tutorial/ruby-string-methods/)

3. Amplia gama de métodos de transformación de cadenas en Ruby: [https://www.thoughtco.com/string-transformations-in-ruby-2907742](https://www.thoughtco.com/string-transformations-in-ruby-2907742)

Ten en cuenta que este enfoque solo funcionará para las letras del alfabeto inglés. Para otros idiomas, es posible que necesites una solución más compleja que trate con especificidades lingüísticas.