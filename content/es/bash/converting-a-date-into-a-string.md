---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Convertir una fecha en una cadena es llevar una fecha (2022-03-21 por ejemplo) a un formato de texto ("21 de marzo de 2022"). Los programadores hacen esto para manipular y presentar los datos de una manera más legible o fácil de usar.

## Cómo 
En Bash, usamos el comando `date`. Veamos un simple ejemplo.

```Bash
# Convertir la fecha actual a una cadena
fecha=$(date +"%d de %B de %Y")
echo $fecha
```

Salida:
```Bash
21 de marzo de 2022
```

Este código toma la fecha actual y la convierte en una cadena en español, como "21 de marzo de 2022".

## Análisis Profundo
El comando `date` ha sido un pilar en Unix y Linux durante décadas, permitiendo a los programadores manejar fechas y tiempo. Hay otras formas de convertir una fecha en una cadena, por ejemplo utilizando Python, Perl o Ruby, pero Bash ofrece un método sencillo y directo.

Aunque `date` es fácil de usar, también es sorprendentemente potente. Puedes formatear la salida de `date` de muchas formas diferentes usando diversos formatos. Echa un vistazo a la página man de date (`man date`) para conocer todos sus secretos.

## Ver También
Bash manual - [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)   
Manual de `date` - [https://www.man7.org/linux/man-pages/man1/date.1.html](https://www.man7.org/linux/man-pages/man1/date.1.html)   
Advanced Bash-Scripting Guide - [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)