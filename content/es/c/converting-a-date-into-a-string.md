---
title:    "C: Convirtiendo una fecha en una cadena"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena en C?

Al trabajar con fechas en un programa en C, es importante poder mostrarlas en un formato legible y fácil de entender para el usuario final. Convertir una fecha en una cadena de caracteres nos permite lograr esto. En este artículo, aprenderás cómo convertir una fecha en una cadena en C utilizando código de ejemplo y profundizarás en cómo funciona este proceso.

## Cómo hacerlo

Para convertir una fecha en una cadena en C, primero debemos obtener los valores de día, mes y año de la fecha. Esto se puede hacer utilizando la función `localtime()` de la biblioteca `time.h`. A continuación, utilizaremos la función `sprintf()` para formatear estos valores en una cadena. Aquí hay un ejemplo de código:

```C
#include <stdio.h>
#include <time.h>

int main(){
    
    // Obtener la fecha actual del sistema
    time_t now = time(NULL);
    
    // Convertir a estructura de tiempo
    struct tm *date = localtime(&now);
    
    // Obtener los valores de día, mes y año
    int day = date->tm_mday;
    int month = date->tm_mon + 1;
    int year = date->tm_year + 1900;
    
    // Convertir a una cadena en formato DD-MM-AAAA
    char date_string[11];
    sprintf(date_string, "%02d-%02d-%04d", day, month, year);
    
    // Imprimir la cadena resultante
    printf("La fecha actual es: %s \n", date_string);
    
    return 0;
}
```

El código anterior imprimirá la fecha actual en formato DD-MM-AAAA, por ejemplo: `La fecha actual es: 16-11-2021`.

## Profundizando

La función `sprintf()` utilizada en el ejemplo anterior es una función de la biblioteca estándar de C que nos permite formatear una cadena de caracteres. Su primer argumento es un puntero a una cadena, y los argumentos siguientes son valores que se colocarán en esa cadena.

En nuestro caso, utilizamos `%02d` para indicar que el valor debe tener un ancho mínimo de dos caracteres y debe tener un cero delante si es necesario. De esta forma, si el día es menor a 10, la cadena resultante incluirá el cero delante y tendrá siempre dos dígitos. El resto de los argumentos se utilizan para obtener los valores de la fecha y se colocan en el orden deseado en la cadena resultante.

Es importante tener en cuenta que al utilizar la función `localtime()` para obtener los valores de la fecha, estos pueden variar dependiendo del sistema operativo y la configuración regional. Por lo tanto, es recomendable utilizar la función `snprintf()` en su lugar, ya que es más segura que `sprintf()` y tiene la ventaja de poder especificar la longitud máxima de la cadena de destino.

## Ver también

- [Función localtime()](https://es.cppreference.com/w/c/chrono/localtime)
- [Función sprintf()](https://es.cppreference.com/w/c/io/fprintf)
- [Función snprintf()](https://es.cppreference.com/w/c/io/snprintf)