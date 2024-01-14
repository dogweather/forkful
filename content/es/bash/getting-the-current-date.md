---
title:    "Bash: Obteniendo la fecha actual."
keywords: ["Bash"]
---

{{< edit_this_page >}}

## ¿Por qué deberías aprender a obtener la fecha actual?

No importa si eres un principiante o un programador experimentado, siempre es importante saber cómo obtener la fecha actual en tus scripts de Bash. Esto puede ser útil para generar nombres de archivos únicos, imprimir la fecha en un archivo de registro o simplemente para obtener información sobre cuándo se ejecutó un comando.

## Cómo hacerlo

Para obtener la fecha actual en Bash, puedes utilizar el comando `date`. Este comando acepta diferentes opciones para personalizar el formato de salida de la fecha. Veamos un ejemplo sencillo de cómo obtener la fecha actual en formato ISO 8601:

```Bash
date +%F
```

Este comando imprimirá la fecha en el formato "AAAA-MM-DD", por ejemplo:

```Bash
2022-01-23
```

Pero ¿qué sucede si queremos obtener la fecha y la hora? Podemos utilizar la opción `-u` para obtener la hora en tiempo universal coordinado (UTC) y la opción `%T` para especificar el formato de hora de 24 horas. El comando se vería así:

```Bash
date -u +%FT%T
```

Y la salida sería:

```Bash
2022-01-23T13:26:45
```

Estos son solo algunos ejemplos de cómo puedes personalizar la salida de fecha y hora en Bash. Puedes experimentar con diferentes opciones y encontrar la que mejor se adapte a tus necesidades.

## Profundizando

Ahora que ya sabes cómo obtener la fecha actual en Bash, vamos a profundizar un poco más en el comando `date`. Una cosa importante a tener en cuenta es que el resultado del comando `date` puede variar dependiendo de la configuración regional del sistema. Por ejemplo, si tu sistema está en español, la salida será en español también.

Puedes cambiar el idioma de salida utilizando la variable `LANG`. Por ejemplo, si quieres obtener la fecha en inglés, puedes ejecutar el comando así:

```Bash
LANG=en_US.utf8 date
```

La salida será en inglés, como por ejemplo:

```Bash
Sun Jan 23 13:28:57 UTC 2022
```

Además, si estás utilizando `date` en un script de Bash y necesitas almacenar la fecha actual en una variable, puedes hacerlo de la siguiente manera:

```Bash
fecha_actual=$(date +%F)
```

Esto almacenará la fecha actual en la variable `fecha_actual`, que luego puedes utilizar en tu script.

## Ver también

- [Documentación de `date` en GNU.org](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Ejemplos de `date` en la línea de comandos en TecMint](https://www.tecmint.com/linux-date-command-examples/)

¡Felicidades! Ahora sabes cómo obtener la fecha actual en Bash y has aprendido algunos trucos para personalizar la salida según tus necesidades. No dudes en seguir aprendiendo y experimentando con diferentes opciones de `date` para hacer tus scripts más eficientes. ¡Hasta la próxima!