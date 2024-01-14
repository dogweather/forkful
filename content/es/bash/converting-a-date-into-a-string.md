---
title:    "Bash: Convirtiendo una fecha en una cadena"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena de texto?

Cuando trabajamos con fechas en un programa de Bash, a veces necesitamos convertirlas en formato de texto para poder manipularlas y mostrarlas de manera adecuada en nuestras aplicaciones. 

## Cómo hacerlo:

Para convertir una fecha en una cadena de texto en Bash, podemos usar el comando `date` junto con los parámetros adecuados. Veamos un ejemplo:

```Bash
fecha=$(date +"%d/%m/%Y")
echo "Hoy es $fecha" 
```

El comando `date` con el parámetro `+%d/%m/%Y` nos permite obtener la fecha actual en el formato de día/mes/año. Luego, asignamos esa fecha a una variable llamada `fecha` y la imprimimos utilizando `echo`. 

La salida de este código sería:

```
Hoy es 26/09/2021
```

## Profundizando:

Si queremos convertir una fecha específica en una cadena de texto, podemos usar el parámetro `-d` junto con la fecha en formato ISO 8601. Por ejemplo:

```Bash
fecha=$(date -d 2021-09-26 +"%d de %B del %Y")
echo "Hoy es $fecha"
```

En este caso, estamos convirtiendo la fecha "2021-09-26" en una cadena de texto con el formato "26 de Septiembre del 2021". La salida sería:

```
Hoy es 26 de Septiembre del 2021
```

Además, también podemos utilizar el comando `date` para formatear fechas y horas en diferentes zonas horarias o idiomas, lo que lo hace una herramienta muy útil y versátil en nuestras tareas de desarrollo.

## Ver también:

- [Conversión de fechas en Bash (en inglés)](https://www.baeldung.com/linux/date-command)
- [Guía rápida de los parámetros de `date` (en inglés)](https://www.tutorialspoint.com/unix_commands/date.htm)
- [Especificación ISO 8601 (en español)](https://es.wikipedia.org/wiki/ISO_8601)