---
title:                "Bash: Concatenando cadenas"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica útil en la programación que permite combinar diferentes cadenas de texto en una sola cadena. Esta habilidad es especialmente útil cuando se trabaja con variables y se necesita mostrar un mensaje completo en la pantalla. Aprender a concatenar cadenas en Bash te ayudará a escribir scripts más dinámicos y eficientes.

## Cómo hacerlo

La concatenación de cadenas en Bash se realiza usando el símbolo de punto (".") para unir dos o más cadenas. Por ejemplo:

```Bash
primera = "Hola"
segunda = "mundo"
echo $primera.$segunda
```
Salida: Hola mundo

En este ejemplo, se ha creado una variable llamada "primera" con el valor "Hola" y otra variable llamada "segunda" con el valor "mundo". Luego, se utiliza el comando "echo" para imprimir en pantalla una combinación de ambas cadenas, unidas por el símbolo de punto.

También es posible concatenar cadenas con variables ya existentes en lugar de escribir el texto directamente. Por ejemplo:

```Bash
nombre = "María"
apellido = "García"
echo "¡Hola, mi nombre es $nombre $apellido!"
```
Salida: ¡Hola, mi nombre es María García!

Aquí, se han utilizado las variables "nombre" y "apellido" dentro de una cadena entre comillas. Al utilizar el símbolo de dólar y los corchetes, se puede acceder al valor de la variable al imprimir el mensaje.

## Profundizando

La concatenación de cadenas en Bash también nos permite utilizar comillas simples ('') y dobles ("") para tener un mayor control sobre cómo se muestran las cadenas y cómo se interpretan los caracteres especiales. Por ejemplo:

```Bash
pais = "España"
echo '$pais es un país hermoso'
```
Salida: $pais es un país hermoso

```Bash
pais = "España"
echo "$pais es un país hermoso"
```
Salida: España es un país hermoso

Aquí se nota que en la primera línea el carácter dólar y la variable no son interpretados, mientras que en la segunda línea sí lo son.

También es posible concatenar cadenas con el resultado de un comando utilizando la expresión de subshell "$( )". Por ejemplo:

```Bash
fecha = $(date +%d/%m/%Y)
echo "Hoy es $fecha"
```
Salida: Hoy es 16/09/2021

Esto puede ser especialmente útil al automatizar tareas y obtener información actualizada.

## Ver también

- [Tutorial de concatenación de cadenas en Bash](https://www.hostinger.es/tutoriales/bash-concatenar-cadenas/)
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/)
- [Ejemplos prácticos de concatenación de cadenas en Bash](https://likegeeks.com/es/ejemplos-de-programacion-de-shell-scripting-en-bash/)