---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

---

## ¿Qué & Por Qué?
En programación, parsear una fecha desde una cadena es convertir un valor string a un objeto de fecha válido. Los programadores suelen hacerlo para almacenar, comparar o manipular fechas más eficazmente en el código.

## Cómo se hace:
Aquí está un código sencillo de muestra en C para parsear una fecha desde una cadena.

```C
#include<stdio.h>
#include<time.h>

int main() {
    char cadena_fecha[10];
    struct tm fecha = {0};

    printf("Ingrese la fecha (dd-mm-aaaa): ");
    fgets(cadena_fecha, 10, stdin);

    strptime(cadena_fecha, "%d-%m-%Y", &fecha);

    printf("Fecha: \nDia: %d \nMes: %d \nAño: %d", 
    fecha.tm_mday, fecha.tm_mon + 1, fecha.tm_year + 1900);
  
    return 0;
}
```
Este programa solicita al usuario una fecha, la procesa y la muestra separada en día, mes y año.

## Profundización
Este proceso de parseo de fechas tiene más historia de la que parece a primera vista. Tradicionalmente, las fechas se representaban como cadenas en la mayoría de las aplicaciones. Sin embargo, esto presentaba problemas cuando se requería hacer cálculos o comparaciones. Así surgió la necesidad de una forma útil de convertir las cadenas de texto a objetos de fecha.

Existen alternativas al método presentado. Por ejemplo, la función sscanf puede ser usada para descomponer una cadena, aunque implica una mayor complejidad y riesgo de errores.

Algo a tener en cuenta sobre strptime es que, según el compilador que se utilice, es posible que no admire todas las directivas de formato. En este caso, puede requerir un tratamiento adicional de las fechas.

## Ver También
Para profundizar en este tópico, podrías echar un vistazo a los siguientes recursos:

- Manual GNU libc sobre fecha y tiempo: [https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html)
- Stack Overflow acerca de parseo de fecha: [https://stackoverflow.com/questions/321849/strptime-equivalent-in-c](https://stackoverflow.com/questions/321849/strptime-equivalent-in-c)