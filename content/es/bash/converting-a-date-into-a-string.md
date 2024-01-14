---
title:    "Bash: Convirtiendo una fecha en una cadena"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

¿Has encontrado alguna vez la necesidad de convertir una fecha en una cadena de texto en tus scripts de Bash? Aunque puede parecer un concepto simple, es una tarea común en la programación que puede ahorrar tiempo y mejorar la eficiencia. En este artículo te explicaremos cómo hacerlo y profundizaremos en el proceso detrás de la conversión.

## Cómo hacerlo

La forma más sencilla de convertir una fecha en una cadena de texto en Bash es utilizando el comando `date`. Veamos un ejemplo:

```Bash
# Muestra la fecha actual en formato de cadena de texto
fecha_actual=$(date +"%A, %d de %B de %Y")
echo $fecha_actual
```

La salida de este script sería algo como: "Lunes, 12 de Julio de 2021". Aquí, el código `%A` representa el día de la semana, `%d` el día del mes, `%B` el nombre del mes y `%Y` el año en formato de cuatro dígitos. Puedes personalizar el formato utilizando diferentes códigos y combinaciones, dependiendo de tus necesidades.

Otra forma de realizar la conversión es utilizando la función `strftime` en un script de Bash:

```Bash
# Crea la fecha como un valor de tiempo
fecha=$(date --date="2021-07-12" +%s)

# Utiliza strftime para convertir la fecha en una cadena
fecha_convertida=$(strftime "%A, %d de %B de %Y" "$fecha")
echo $fecha_convertida
```

La salida sería la misma que en el ejemplo anterior, aunque esta forma te permite trabajar con fechas específicas en lugar de sólo la fecha actual.

## Profundizando

Cuando conviertes una fecha en una cadena de texto, en realidad estás realizando una operación de formato. En Bash, los comandos `date` y `strftime` utilizan un formato de fecha y hora conocido como Unix Time o Epoch Time. Este formato cuenta los segundos desde el 1 de enero de 1970, conocido como el "epoch". Al utilizar diferentes combinaciones de códigos en tu comando de conversión, estás extrayendo diferentes partes del tiempo almacenado en una variable Unix Time.

Además de las opciones que ya hemos mencionado, existen otros códigos que puedes utilizar para personalizar el formato de tu cadena de fecha, como `%H` para la hora en formato de 24 horas, `%M` para los minutos, o `%z` para la zona horaria, entre otros.

En resumen, la conversión de una fecha a una cadena de texto en Bash es una tarea útil y común en la programación, que puede ser realizada rápidamente con los comandos `date` y `strftime`. Ahora que conoces el proceso detrás de la conversión, puedes personalizarla según tus necesidades y mejorar tus scripts de Bash.

## Ver también

- [Documentación de la función `date` en Linux](https://linux.die.net/man/1/date)
- [Documentación de la función `strftime` en Linux](https://linux.die.net/man/3/strftime)