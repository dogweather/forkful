---
title:    "Bash: Escribiendo en el error estándar"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por qué

La escritura en el estándar de error es una herramienta esencial para cualquier programador de Bash. Al enviar mensajes de error y salida de depuración al estándar de error, podemos asegurarnos de que nuestra aplicación sea más fácil de depurar y más robusta en general.

## Como hacerlo

Para escribir al estándar de error en Bash, simplemente utilizamos el operador `>&2`. Aquí hay un ejemplo sencillo:

```Bash
echo "Este es un mensaje de error" >&2
```

Esto enviará el mensaje "Este es un mensaje de error" al estándar de error. También podemos combinar la salida estándar y estándar de error en un solo archivo usando el operador `2>&1`. Por ejemplo:

```Bash
ls -l archivo_inexistente 2>&1 > lista_archivos.txt
```

Esto ejecutará el comando `ls -l archivo_inexistente` y redirigirá tanto la salida estándar como el estándar de error al archivo `lista_archivos.txt`.

## Profundizando

Cuando trabajamos con Bash, es importante recordar que el estándar de error no es solo útil para mensajes de error. También se puede utilizar para imprimir mensajes de depuración y otros tipos de salida que no queremos que se muestren en la consola. Además, podemos redirigir el estándar de error a otro archivo que no sea la consola, lo que puede resultar especialmente útil al trabajar en scripts.

El estándar de error también puede ser utilizado en combinación con herramientas de depuración como `xtrace` y `set -e` para obtener información útil sobre dónde se producen errores en nuestro código.

## Ver también

- [Cómo escribir en el estándar de error en Bash](https://www.shellhacks.com/write-stderr-standard-error-file-bash/)
- [Guía de Bash para principiantes](https://www.freecodecamp.org/news/the-ultimate-guide-to-bash-command-line-2982189c4c9)
- [Redirección en Bash](https://www.gnu.org/software/bash/manual/html_node/Redirections.html) #Español