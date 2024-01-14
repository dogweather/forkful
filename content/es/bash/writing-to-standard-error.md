---
title:                "Bash: Escritura en la salida de error estándar"
simple_title:         "Escritura en la salida de error estándar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías escribir en la salida de error estándar (stderr)?

La mayoría de los programas de Bash escriben sus errores en la salida de error estándar (stderr) en lugar de en la salida estándar (stdout). Esto se debe a que stderr está diseñado específicamente para mostrar mensajes de error, mientras que stdout se utiliza para mostrar resultados y salidas normales del programa. Escribir en stderr puede ser útil cuando estás depurando un programa y quieres ver los mensajes de error en tiempo real.

## Cómo escribir en la salida de error estándar

Para escribir en stderr en Bash, simplemente tienes que agregar "2>" al final del comando que quieras ejecutar. Por ejemplo, si queremos ver los errores de un comando de copia, podemos escribir:

```Bash
cp archivo_inexistente.txt archivo_nuevo.txt 2> errores.txt
```

Esto redirigirá todos los mensajes de error a un archivo llamado "errores.txt" en lugar de imprimirlos en la terminal. También puedes redirigir stderr a stdout utilizando "2>&1". Así que si queremos ver tanto los resultados como los errores de nuestro comando de copia, podemos escribir:

```Bash
cp archivo_inexistente.txt archivo_nuevo.txt 2>&1
```

Puedes probar esto con diferentes comandos y ver cómo funciona cada vez.

## Profundizando en la salida de error estándar

Es importante mencionar que stderr no sólo se utiliza para mostrar mensajes de error, sino también para mostrar otros tipos de mensajes importantes, como advertencias o mensajes de estado. Además, stderr no está limitado a ser redirigido a un archivo, sino que también puede ser utilizado para enviar mensajes a otros procesos.

Otra cosa a tener en cuenta es que stderr y stdout son tratados de manera diferente por Bash. Por ejemplo, si ejecutas un programa en el que la salida de error provoca que el programa se detenga, puedes encontrar que la salida se intercala en la pantalla, lo cual puede ser difícil de leer. Esto se debe a que Bash no tiene un mecanismo para sincronizar stdout y stderr. Puedes solucionar esto utilizando el comando "tee" para duplicar la salida en un archivo y en la pantalla al mismo tiempo.

En resumen, escribir en la salida de error estándar puede ser útil en múltiples situaciones para depurar y gestionar los mensajes de error de un programa. Asegúrate de probar diferentes métodos de redirección y de entender cómo Bash trata a stderr para evitar confusiones en tus programas.

## Ver también

- [Documentación de Bash sobre la salida de error estándar](https://www.gnu.org/software/bash/manual/html_node/Redirections.html) 
- [Tutorial de Bash sobre la salida de error estándar](https://www.howtogeek.com/435903/what-is-teaching-kids-to-code-on-a-raspberry-pi/) 
- [Explicación de stackoverflow sobre la diferencia entre stderr y stdout](https://stackoverflow.com/questions/230751/how-to-display-stderr-with-colour-on-screen)