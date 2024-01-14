---
title:    "Bash: Cambiando a mayúsculas una cadena"
keywords: ["Bash"]
---

{{< edit_this_page >}}

Blogs de programación casual en Bash para lectores españoles

## ¿Por qué capitalizar una cadena en Bash?

Capitalizar una cadena de texto es un proceso común en la programación, ya que ayuda a mejorar la legibilidad y organización del código. Al convertir todas las letras de una cadena en mayúsculas, se facilita la identificación de palabras clave y estructuras importantes dentro del código. En Bash, hay varias formas de lograr esto y en este blog, discutiremos cómo hacerlo.

## Cómo capitalizar una cadena en Bash

Existen diferentes métodos para capitalizar una cadena en Bash, cada uno con sus propias ventajas y desventajas. A continuación se presentan tres ejemplos utilizando distintas técnicas.

### Enfoque utilizando las herramientas internas de Bash

Este enfoque utiliza las herramientas internas de Bash para capitalizar una cadena. Primero, se asigna la cadena a una variable llamada "cadena". Luego, se utiliza el comando "echo" para imprimir la variable y se redirige la salida a un archivo temporal. A continuación, se utiliza el comando "tr" para convertir todas las letras en minúscula y el comando "sed" para convertir la primera letra en mayúscula. Finalmente, se lee el contenido del archivo temporal y se asigna a una nueva variable llamada "cadenaCapitalizada". El resultado final es una cadena en mayúsculas.

```Bash
cadena="ejemplo de cadena"
echo "$cadena" > temp.txt
cadenaCapitalizada=$(cat temp.txt | tr '[:lower:]' '[:upper:]' | sed 's/.*/\u&/')
```

El resultado de "echo $cadenaCapitalizada" será "Ejemplo de cadena".

### Enfoque utilizando el comando "tr"

Este enfoque utiliza solo el comando "tr" para capitalizar la cadena. Se asigna la cadena a una variable y se utiliza el comando "tr" para convertir las letras en minúscula. Luego, se utiliza un patrón de expresión regular para convertir la primera letra en mayúscula. El resultado final se asigna a una nueva variable llamada "cadenaCapitalizada".

```Bash
cadena="ejemplo de cadena"
cadenaCapitalizada=$(echo "$cadena" | tr '[:lower:]' '[:upper:]'| sed 's/\b./\u&/g')
```

El resultado de "echo $cadenaCapitalizada" será "Ejemplo de cadena".

### Enfoque utilizando la función "capitalize" de Bash

Este enfoque utiliza una función llamada "capitalize" que capitaliza cada palabra en una cadena. Primero, se declara la función y se asigna la cadena a una variable. Luego, se llama a la función con la variable como argumento y se asigna el resultado a una nueva variable llamada "cadenaCapitalizada". El resultado final es una cadena con cada palabra en mayúscula.

```Bash
capitalize() {
    local string=${1,,}
    echo ${string^}
}
cadena="ejemplo de cadena"
cadenaCapitalizada=$(capitalize "$cadena")
```

El resultado de "echo $cadenaCapitalizada" será "Ejemplo De Cadena".

## Profundizando en la capitalización de cadenas en Bash

Como se puede ver, hay varias formas de capitalizar una cadena en Bash. Además de los ejemplos mencionados anteriormente, existen otras herramientas y técnicas que se pueden utilizar para lograr el mismo resultado. También es importante tener en cuenta que estos métodos pueden variar dependiendo de la versión de Bash que se esté utilizando.

Para profundizar en este tema, te recomendamos revisar la documentación oficial de Bash y experimentar con diferentes enfoques para capitalizar cadenas en tus propios proyectos.

## Ver también

- [Documentación oficial de Bash](https://www.gnu.org/software/bash/)
- [Tutorial de Bash para principiantes](https://linuxize.com/post/bash-for-loop/)
- [Ejemplos de Bash en Codecademy](https://www.codecademy.com/courses/learn-bash/lessons/commands/exercises/echo-i?action=lesson_resume)