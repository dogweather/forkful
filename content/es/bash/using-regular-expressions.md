---
title:    "Bash: Utilizando expresiones regulares"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por qué

Si eres un programador o alguien que trabaja con procesamiento de datos, es probable que hayas escuchado acerca de las expresiones regulares ("regex" por sus siglas en inglés). Las expresiones regulares son una herramienta poderosa y versátil para manipular texto y patrones en archivos y programas. Con regex, puedes buscar y reemplazar cadenas de caracteres, validar formatos de datos y mucho más. ¡Sigue leyendo para aprender cómo usarlas en tus propios proyectos!

## Cómo hacerlo

Para utilizar expresiones regulares en Bash, primero tienes que asegurarte de tener la herramienta `grep` instalada en tu sistema. `grep` es una herramienta de línea de comandos que se utiliza para buscar texto en archivos o flujos de datos. Una vez que tienes `grep` instalado, puedes comenzar a usar expresiones regulares.

Imaginemos que tienes un archivo de texto llamado `mi_archivo.txt` que contiene una lista de correos electrónicos. Quieres encontrar todos los correos electrónicos que terminan con "@miempresa.com". Podrías escribir el siguiente comando en la terminal:

```
$ grep ".*@miempresa\.com" mi_archivo.txt
```

En este ejemplo, la expresión regular que estamos pasando a `grep` es `.*@miempresa\.com`. Esta expresión regular usa varios caracteres especiales, como el punto (.), el asterisco (*) y la barra invertida (\). Estos caracteres especiales tienen un significado específico al trabajar con expresiones regulares. Por ejemplo, el punto representa cualquier carácter, excepto una nueva línea, mientras que el asterisco significa que el carácter anterior puede aparecer cero o más veces. La barra invertida se utiliza para escapar de caracteres especiales y permitir que sean interpretados literalmente.

En el ejemplo anterior, la expresión regular `.*@miempresa\.com` significa "cualquier carácter (.) seguido de cero o más repeticiones (*) de cualquier carácter (.), seguido de '@miempresa.com'".

## Profundizando

Las expresiones regulares pueden ser mucho más complejas que el ejemplo anterior. Pueden incluir conjuntos de caracteres, cuantificadores, agrupaciones y más. También hay diferentes variantes de expresiones regulares, como las utilizadas en Perl o las utilizadas en Python.

Para dominar verdaderamente las expresiones regulares, es útil tener una buena comprensión de los diferentes elementos y cómo funcionan juntos. Un buen recurso para aprender más es el libro "Mastering Regular Expressions" de Jeffrey Friedl.

## Ver también

- [Bash One-Liners - `grep` and Regular Expressions](https://catonmat.net/bash-one-liners-explained-part-four)
- [Expresiones regulares: la guía definitiva](https://codingornot.com/2016/04/04/expressions-regulares/)
- [Ejemplos de expresiones regulares en Bash](https://linuxhint.com/reg_expressions_regular_expressions_examples/)