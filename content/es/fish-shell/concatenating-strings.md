---
title:    "Fish Shell: Concatenando cadenas"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué concatenar strings en Fish Shell?

Concatenar strings es una función importante en la programación que permite combinar múltiples cadenas de texto en una sola. Puede ser utilizado para generar mensajes personalizados, crear nombres de archivos dinámicos o simplemente para organizar mejor la información. En esta entrada, aprenderemos cómo utilizar la función de concatenación en Fish Shell.

## Cómo utilizar la concatenación de strings en Fish Shell

La sintaxis para concatenar strings en Fish Shell es bastante sencilla. Simplemente tienes que utilizar el operador de suma (+) entre dos cadenas de texto para unirlas. Veamos un ejemplo:

```Fish Shell
set primero "Hola"
set segundo "mundo"
echo $primero$segundo
```
Output: Hola mundo

En el ejemplo anterior, definimos dos variables, "primer" y "segundo", y luego utilizamos el operador de suma para unirlos e imprimir el resultado con el comando "echo". También puedes utilizar el operador de suma para unir múltiples cadenas de texto en una sola, como en el siguiente ejemplo:

```Fish Shell
set nombre "Maria"
set apellido "García"
set saludo "Hola "
echo $saludo$nombre" "$apellido". ¿Cómo estás?"
```
Output: Hola Maria García. ¿Cómo estás?

Como puedes ver, también puedes agregar texto adicional entre las variables para crear una cadena de texto más compleja. Además, puedes utilizar el comando "set -l" para definir una variable local y utilizarla dentro de un bloque de código, como en este ejemplo:

```Fish Shell
set -l mensaje "Buenos días "
for i in 1 2 3
set saludo $mensaje$i
echo $saludo
end
```
Output: Buenos días 1
Buenos días 2
Buenos días 3

## Profundizando en la concatenación de strings

Además del operador de suma, también puedes utilizar la función "string join" para unir múltiples cadenas de texto con un separador específico, como en este ejemplo:

```Fish Shell
set nombres "Maria" "Juan" "Ana"
echo (string join ", " $nombres)" son amigos."
```
Output: Maria, Juan, Ana son amigos.

También puedes utilizar la función "contains" para verificar si una cadena de texto contiene otra cadena, lo cual puede ser útil al concatenar strings. Por ejemplo:

```Fish Shell
set palabra "perro"
set oracion "El perro ladra."
if contains $oracion $palabra
echo "La oración contiene la palabra $palabra."
end
```
Output: La oración contiene la palabra perro.

Otra función útil es "string replace", que te permite reemplazar una parte de una cadena de texto por otra. Por ejemplo:

```Fish Shell
set oracion "Estoy paseando al perro."
set perro "gato"
echo (string replace $oracion $palabra $gato)
```
Output: Estoy paseando al gato.

## Ver también
- [Guía para principiantes de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Concatenar strings en otros lenguajes de programación](https://www.geeksforgeeks.org/how-to-concatenate-two-strings-in-java/)