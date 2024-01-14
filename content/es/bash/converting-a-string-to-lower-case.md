---
title:                "Bash: Convirtiendo una cadena a minúsculas"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas puede ser útil cuando se trabaja con programación de Bash. Esto permite que el programa reconozca la misma cadena en diferentes formas, ya sea en mayúsculas o minúsculas.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Bash, se puede utilizar el comando "tr". Este comando permite transformar un conjunto de caracteres en otro. En este caso, utilizaremos "tr" para transformar los caracteres en mayúscula a caracteres en minúscula.

```Bash
cadena="BLOG DE PROGRAMACIÓN EN BASH"
echo $cadena | tr '[:upper:]' '[:lower:]'
# Output: blog de programación en bash
```

Como se puede ver en el ejemplo, utilizamos el comando "tr" junto con los parámetros "[:upper:]" y "[:lower:]" para indicarle que queremos transformar los caracteres en mayúscula a caracteres en minúscula.

## Profundizando

Al usar el comando "tr" para convertir una cadena de texto a minúsculas, es importante tener en cuenta que este comando no solo afectará a las letras, sino a cualquier carácter que esté en mayúscula. Por ejemplo, si nuestra cadena incluye símbolos o números en mayúscula, también serán transformados a minúscula.

Además, este comando solo afectará a la cadena de texto que se le indique, no cambiará permanentemente en la variable original. Por lo tanto, si queremos utilizar la cadena convertida a minúsculas, debemos almacenarla en una nueva variable o imprimir el resultado directamente.

## Ver también

Si quieres seguir aprendiendo sobre programación en Bash, aquí te dejamos algunos enlaces útiles:

- [Introducción a Bash en español](http://www.solucione.es/introduccion-a-bash/)
- [Guía de comandos básicos de Bash](https://www.hostinger.es/tutoriales/comandos-basicos-de-linux/)
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/)

¡Esperamos que este artículo te haya sido útil! Recuerda que practicando y experimentando con los comandos de Bash podrás mejorar tus habilidades y conocimientos en programación. ¡Hasta la próxima!