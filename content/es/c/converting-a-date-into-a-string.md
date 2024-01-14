---
title:    "C: Convirtiendo una fecha en una cadena de texto"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por qué
Convertir una fecha en una cadena de caracteres es una tarea común en la programación en C. Ya sea que estés creando una aplicación de calendario o simplemente necesites mostrar una fecha en un formato específico, saber cómo realizar esta conversión es una habilidad importante para cualquier programador de C.

## Cómo hacerlo
Para convertir una fecha en una cadena de caracteres en C, utilizaremos la función `strftime()` de la biblioteca estándar `time.h`. Esta función nos permite formatear una fecha y hora en una cadena de acuerdo a un formato específico.

Veamos un ejemplo de cómo utilizar `strftime()`:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Obtener el tiempo actual
    time_t now = time(NULL);

    // Crear una estructura de tipo tm con la fecha y hora actual
    struct tm *current_time = localtime(&now);

    // Crear un buffer de caracteres de 50 bytes para almacenar la fecha formateada
    char buffer[50];

    // Utilizar strftime() para formatear la fecha y hora actual en una cadena
    // El formato utilizado es "%A, %d de %B de %Y a las %H:%M:%S"
    strftime(buffer, 50, "%A, %d de %B de %Y a las %H:%M:%S", current_time);

    // Imprimir la cadena formateada
    printf("La fecha y hora actual es: %s\n", buffer);

    return 0;
}
```

Este código imprimirá lo siguiente:

```
La fecha y hora actual es: jueves, 04 de marzo de 2021 a las 11:39:23
```

Como puedes ver, hemos utilizado la función `strftime()` para formatear la fecha y hora actual en una cadena de acuerdo a un formato específico.

## Una mirada más profunda
La función `strftime()` es una función muy versátil que nos permite formatear fechas y horas en una amplia gama de formatos. Aquí hay algunos ejemplos de formatos que puedes utilizar:

- `%H:%M:%S` para mostrar la hora en formato de 24 horas.
- `%I:%M:%S %p` para mostrar la hora en formato de 12 horas con la indicación AM o PM.
- `%d/%m/%y` para mostrar la fecha en formato DD/MM/YY.
- `%Y-%m-%d` para mostrar la fecha en formato YYYY-MM-DD.

Puedes encontrar una lista completa de los formatos disponibles en la documentación de la función `strftime()`.

Una cosa importante a tener en cuenta es que el resultado de `strftime()` dependerá del idioma y las configuraciones locales de tu sistema operativo. Por ejemplo, si utilizas la configuración en español, el resultado de la función puede ser "jueves" en lugar de "Thursday". Si necesitas que tu programa sea compatible con diferentes idiomas, es importante tener esto en cuenta y realizar pruebas exhaustivas.

## Ver también
- [Documentación de la función `strftime()` en C](https://www.cplusplus.com/reference/ctime/strftime/)
- [Tutorial sobre fechas y horas en C](https://www.programiz.com/c-programming/c-date-time)