---
title:                "Escribiendo en el error estándar"
html_title:           "Bash: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué lo hacemos?

Escribir en el flujo de error estándar es una forma en la que los programadores pueden comunicar información de error mientras se ejecuta un script en Bash. Esto es útil para saber qué partes del código están causando problemas y depurarlos más fácilmente.

## Cómo hacerlo:

Para escribir en el flujo de error estándar, se utiliza el comando `echo` seguido por el símbolo `>&2` para indicar que se debe escribir en el flujo de error estándar en lugar del flujo de salida estándar. Por ejemplo:

```Bash
echo "Este es un mensaje de error" >&2
```

Esto imprimirá "Este es un mensaje de error" en la consola de error en lugar de la consola de salida. También se puede utilizar el comando `printf` para escribir en el flujo de error estándar:

```Bash
printf "Este es otro mensaje de error\n" >&2
```

El resultado será el mismo que antes, ya que se está utilizando el símbolo `>&2` para redirigir el mensaje al flujo de error estándar.

## Profundizando:

La escritura en el flujo de error estándar se ha utilizado durante mucho tiempo en la programación de Bash para informar sobre errores y depurar scripts. Sin embargo, existen alternativas como el uso de comandos `exit` con códigos de retorno personalizados o el uso de la función `stderr` en Bash.

En términos de implementación, escribir en el flujo de error estándar se realiza mediante la redirección de salida utilizando el símbolo `>&2` como se mencionó anteriormente. Esto se puede hacer en un solo comando o se puede establecer una redirección global para toda la secuencia de comandos.

## Ver también:

- [Documentación oficial de redirección en Bash](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Explicación detallada de redirección en Bash](https://www.tldp.org/LDP/abs/html/io-redirection.html)