---
title:    "Bash: Escribiendo al error estándar"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir a la salida estándar de errores?

Escribir a la salida estándar de errores es una técnica común en la programación Bash. Permite a los desarrolladores mostrar mensajes de error y depurar su código de manera más eficiente. Al escribir a la salida estándar de errores, se pueden identificar y solucionar problemas más rápidamente, lo que a su vez ayuda a mejorar la calidad del software.

## Cómo hacerlo:

Para escribir a la salida estándar de errores en Bash, se utiliza el comando "echo" seguido de la opción "-e" y el símbolo ">&2", seguido del mensaje de error entre comillas. Por ejemplo:

```Bash
echo -e "¡Ha ocurrido un error!" >&2
```

Esto imprimirá el mensaje "¡Ha ocurrido un error!" en la salida estándar de errores. También se pueden utilizar variables en el mensaje de error, por ejemplo:

```Bash
archivo="nombre del archivo"
echo -e "El archivo $archivo no pudo ser encontrado" >&2
```

Esto imprimirá "El archivo nombre del archivo no pudo ser encontrado" en la salida estándar de errores.

Otra manera de escribir a la salida estándar de errores es utilizando el comando "cat", que permite imprimir contenido de archivos en la salida estándar. Por ejemplo, si se tiene un archivo llamado "errores.txt" con los mensajes de error, se puede utilizar el siguiente comando para imprimirlos en la salida estándar de errores:

```Bash
cat errores.txt >&2
```

## Profundizando:

Escribir a la salida estándar de errores no solo es útil para imprimir mensajes de error, también es importante para el manejo de excepciones en scripts Bash. Al escribir mensajes de error en la salida estándar de errores, se pueden utilizar comandos como "grep" o "awk" para filtrar y procesar estos errores de manera más eficiente.

Además, es importante tener en cuenta que la salida estándar de errores es diferente a la salida estándar. Mientras que la salida estándar (representada por "1") se utiliza para imprimir resultados o información relevante, la salida estándar de errores (representada por "2") se utiliza específicamente para mensajes de error.

En resumen, escribir a la salida estándar de errores es una técnica valiosa para mejorar la eficiencia y calidad del código en Bash. Al aprender a utilizar correctamente esta herramienta, se pueden identificar y solucionar problemas en el código de manera más rápida y eficiente.

## Ver también:

- [Documentación de Bash sobre la salida estándar de errores](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Artículo sobre el manejo de errores en scripts Bash](https://www.thegeekstuff.com/2010/06/bash-error-handling/)
- [Tutorial sobre cómo utilizar la salida estándar de errores en Bash](https://linuxize.com/post/bash-redirect-stderr-stdout/)