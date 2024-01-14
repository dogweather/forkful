---
title:    "Fish Shell: Encontrando la longitud de una cadena"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

" ## Por qué

La búsqueda de la longitud de una cadena es una tarea común en la programación que puede ser útil al realizar operaciones con cadenas de texto. Saber cómo obtener esta información puede ahorrar tiempo y esfuerzo al manipular cadenas en programas escritos en Fish Shell.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Fish Shell, se puede utilizar el comando `string length`, seguido del nombre de la cadena entre paréntesis.

````Fish Shell
string length "hola mundo"
>>> 11
````

Este comando devuelve el número total de caracteres en la cadena, incluyendo espacios y signos de puntuación.

Otra forma de averiguar la longitud de una cadena es utilizando la variable integrada `$string_length`, que almacena automáticamente la longitud de la cadena más reciente utilizada.

````Fish Shell
set cadena "¡Buenos días!"
echo $string_length
>>> 13
````

También se puede utilizar la función `strlen` para obtener la longitud de una cadena. Sin embargo, esta función solo está disponible en Fish Shell 3.0 y versiones posteriores.

````Fish Shell
set cadena "Hola"
strlen $cadena
>>> 4
````

## Profundizando

Mientras que el comando `string length` puede ser suficiente para la mayoría de las situaciones, es importante tener en cuenta que si la cadena contiene caracteres multibyte, el resultado puede no ser preciso.

Esto se debe a que el comando se basa en el número total de bytes en la cadena y no en el número de caracteres individuales. Para obtener una longitud precisa en este caso, se debe utilizar la función `strlen` mencionada anteriormente.

También es importante tener en cuenta que las funciones y variables mencionadas anteriormente solo están disponibles en Fish Shell y pueden no funcionar correctamente en otros shells de Linux.

## Ver también

- Documentación oficial de Fish Shell: https://fishshell.com/docs/current/
- Tutorial de Fish Shell: https://linuxize.com/post/how-to-install-and-use-fish-shell-on-ubuntu-18-04/
- Guía de comandos de Fish Shell: https://devhints.io/fish