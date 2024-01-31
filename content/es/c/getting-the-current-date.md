---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:13:06.912061-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Obtener la fecha actual en un programa es tan sencillo como preguntarle al sistema "¿qué día es hoy?". Los programadores hacemos esto para registrar eventos, marcar tiempos de ejecución o simplemente para mostrar la fecha al usuario.

## Cómo hacerlo:

```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL);
    struct tm fecha = *localtime(&t);
    
    printf("Hoy es: %d-%02d-%02d\n", fecha.tm_year + 1900, fecha.tm_mon + 1, fecha.tm_mday);
    
    return 0;
}

```

Salida de muestra:

```
Hoy es: 2023-04-10
```

## Análisis Profundo:

En los viejos tiempos de la programación, obtener la fecha no era tan directo como lo es hoy. Antes, dependíamos de sistemas operativos y su manera de entender el tiempo.

`time(NULL)` nos da el tiempo actual en segundos desde la "Epoch" (1 de enero de 1970). La estructura `tm` de `localtime` transforma esos segundos en una forma amigable.

Existen alternativas para obtener la fecha y hora, como `gettimeofday` o bibliotecas externas, pero `time` y `localtime` son suficientes para la mayoría de los casos y vienen estándar en C.

Implementar una función que obtiene la fecha actual es simple, pero hay que tener en cuenta los detalles. Por ejemplo, `tm_year` devuelve los años desde 1900, así que hay que ajustar sumándole 1900. Otro detalle es que `tm_mon` va de 0 a 11, así que sumamos 1 para obtener el mes correcto.

## Ver También:

- Manual de `time.h` en la página de manuales de GNU: [time.h](https://www.gnu.org/software/libc/manual/html_node/Time-Types.html)
- Artículo de Wikipedia sobre la "Epoch" de Unix: [Unix Time](https://en.wikipedia.org/wiki/Unix_time)
- Referencia de la biblioteca estándar C de GNU: [GNU C Library](https://www.gnu.org/software/libc/)
