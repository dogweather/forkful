---
title:    "Bash: Comparando dos fechas"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas?

Comparar dos fechas puede ser una tarea útil en la programación, ya que nos permite determinar la cantidad de tiempo transcurrido entre dos eventos, validar la información de una base de datos o simplemente ordenar datos cronológicamente. Saber cómo comparar correctamente dos fechas puede ahorrarnos tiempo y facilitar nuestro trabajo como desarrolladores.

## Cómo hacerlo

Para comparar dos fechas en Bash, podemos utilizar el comando `date`. Con la opción `-d` podemos especificar la fecha que queremos comparar en un formato específico. Por ejemplo, si queremos comparar dos fechas en formato `YYY-MM-DD`, podemos usar el siguiente código:

```Bash
date -d "2021-01-01" 
date -d "2021-01-05"
```
Esto nos dará como resultado la cantidad de días de diferencia entre ambas fechas. Si queremos obtener información más detallada, podemos utilizar la opción `-u` para obtener el tiempo en UTC o `-I` para mostrar la diferencia en días, horas y minutos.

```Bash
date -d "2021-01-01" -d "2021-01-05" -u
date -d "2021-01-01" -d "2021-01-05" -I
```
Nota: Ten en cuenta que la fecha más reciente debe ir en primer lugar en la comparación.

## Profundizando en la comparación de fechas

En Bash, las fechas se almacenan internamente como segundos desde la medianoche del 1 de enero de 1970. Esto significa que cuando comparamos dos fechas, en realidad estamos comparando dos números. Por lo tanto, para comparar fechas en Bash, debemos asegurarnos de que están en un formato que pueda ser convertido a segundos.

Otra forma de comparar fechas es utilizando el comando `test` y las opciones `-nt` (más reciente) y `-ot` (más antiguo). Esto se utiliza principalmente para comparar la fecha de modificación de archivos.

```Bash
test archivo1 -nt archivo2
test archivo1 -ot archivo2
```

## Ver también

- [Comparing Dates in Bash](https://www.baeldung.com/linux/bash-compare-dates)
- [Bash Date Command](https://linuxize.com/post/bash-date-command/)
- [Comparing and Sorting Dates in Bash](https://superuser.com/questions/926544/comparing-and-sorting-dates-in-bash)