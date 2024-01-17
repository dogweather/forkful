---
title:                "Capitalizando una cadena"
html_title:           "Bash: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Capitalizar una cadena de texto simplemente significa convertir la primera letra de cada palabra a mayúscula. Los programadores a menudo lo hacen para mejorar la legibilidad de su código y para seguir convenciones de codificación establecidas.

## Cómo:

```Bash
# Ejemplo 1: Usando el comando tr para capitalizar una cadena
echo "hola mundo" | tr '[:lower:]' '[:upper:]'
# Salida: HOLA MUNDO

# Ejemplo 2: Usando la función "capitalize" de Bash
texto="esto es una cadena"
texto="${texto^}"
echo $texto
# Salida: Esto es una cadena
```

## Profundizando:

### Contexto histórico:
Capitalizar una cadena de texto puede ser una práctica común en muchos lenguajes de programación, pero en realidad proviene de la imprenta y la tipografía. En la época de los tipos móviles, los mayúsculas eran más grandes y se encontraban en una posición más alta que las minúsculas, por lo que se utilizaban al principio de las oraciones y para destacar ciertas palabras.

### Alternativas:
Además de las opciones mencionadas en la sección "Cómo", también existen otras formas de capitalizar una cadena de texto en Bash. Algunas de ellas incluyen utilizar expresiones regulares con el comando sed y utilizar la función "tr" con variables de entorno.

### Detalles de implementación:
En lenguajes de programación más complejos, como JavaScript o Python, hay funciones específicas para capitalizar cadenas de texto. Pero en Bash, no hay una función dedicada para esto. En su lugar, se pueden utilizar comandos como "tr" o "awk", o aprovechar las secuencias de escape de Bash para realizar la capitalización.

## Ver también:

- Documentación de Bash sobre el uso de expresiones regulares con "sed": https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html
- Stack Overflow: ¿Cómo capitalizar una cadena en Bash?: https://stackoverflow.com/questions/2264428/how-do-i-capitalize-the-first-letter-of-a-string-in-bash
- Historia de la tipografía: https://www.lifewire.com/typography-origin-and-history-1078277