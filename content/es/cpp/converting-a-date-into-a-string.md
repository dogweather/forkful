---
title:                "C++: Convirtiendo una fecha en una cadena"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# ¿Por qué convertir una fecha en una cadena de texto?

Al trabajar con fechas en un programa, es común que necesitemos mostrarlas en formato de cadena de texto. Ya sea para imprimir en pantalla, almacenar en una base de datos o pasar como parámetro a una función, convertir una fecha en una cadena es una tarea esencial en la programación. En este artículo, aprenderemos cómo realizar esta conversión en C++.

## Cómo hacerlo

En C++, existen diversas formas de convertir una fecha en una cadena de texto. A continuación, se presentan dos ejemplos utilizando la librería estándar de C++.

```C++
// Incluir la librería <ctime> para trabajar con fechas
#include <ctime> 

// Función para convertir una fecha en cadena de texto
std::string convertirFecha(int dia, int mes, int anio){
  
  // Crear una estructura de tipo tm y asignar valores a sus campos
  // tm_year - año desde 1900, tm_mon - meses desde 0 (Enero)
  // tm_mday - día del mes, tm_wday - día de la semana
  // tm_yday - día del año, tm_isdst - horario de verano (DST)
  tm fecha = {0, 0, 0, dia, mes-1, anio - 1900, 0, 0, 0};
  
  // Crear un array de caracteres de tamaño suficiente para almacenar la fecha convertida
  char str[80];
  
  // Usar la función strftime para convertir la fecha en una cadena de texto con el formato deseado
  // En este caso, "DD/MM/YYYY"
  strftime(str, 80, "%d/%m/%Y", &fecha);
  
  // Retornar la cadena con la fecha convertida
  return str;
}

int main(){
  // Convertir la fecha 27/10/2021 en una cadena de texto y almacenarla en una variable
  std::string fecha_cadena = convertirFecha(27, 10, 2021);
  
  // Imprimir la cadena en pantalla
  std::cout << fecha_cadena << std::endl;
  
  // Output: 27/10/2021
  return 0;
}
```

Otra forma de realizar la conversión es utilizando la clase `std::stringstream` y la función `std::put_time`.

```C++
// Incluir la librería <ctime> para trabajar con fechas
#include <ctime>
// Incluir la librería <sstream> para utilizar std::stringstream
#include <sstream>

int main(){
  // Crear una estructura de tipo tm y asignar valores a sus campos
  tm fecha = {0, 0, 0, 27, 9, 2021 - 1900, 0, 0, 0};
  
  // Crear un stringstream vacío
  std::stringstream ss;
  
  // Agregar la fecha al stringstream en formato DD/MM/YYYY
  ss << std::put_time(&fecha, "%d/%m/%Y");
  
  // Convertir el stringstream en una cadena de texto
  std::string fecha_cadena = ss.str();
  
  // Imprimir la cadena en pantalla
  std::cout << fecha_cadena << std::endl;
  
  // Output: 27/10/2021
  return 0;
}
```

## Profundizando en la conversión

Ambas formas de convertir una fecha en una cadena de texto utilizan la función `strftime` y la clase `std::stringstream` de la librería `<ctime>`, pero sus implementaciones son diferentes. La función `strftime` toma una estructura `tm` como parámetro y utiliza una cadena de formato para definir cómo se mostrará la fecha en la cadena de texto resultante. Por otro lado, la clase `std::stringstream` permite agregar y manipular datos en un `stringstream` de la misma forma que se haría con `cout` o `cin`.

Es importante tener en cuenta que el resultado de la conversión de una fecha depende del formato de la cadena especificado en la función o clase utilizada. Es recomendable consultar la documentación oficial para encontrar el formato adecuado para mostrar la fecha en la cadena de texto según nuestras necesidades.

## Ver también

- [Documentación oficial de la librería <ctime> en C++](https://cppreference.com/w/cpp/chrono/c/strftime)
- [Tutorial sobre la clase std::stringstream en C++](https://www.learncpp.com/cpp-tutorial/stringstream