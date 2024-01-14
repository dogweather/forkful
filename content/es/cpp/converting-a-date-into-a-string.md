---
title:    "C++: Convirtiendo una fecha en una cadena"
keywords: ["C++"]
---

{{< edit_this_page >}}

# ¿Por qué alguien convertiría una fecha en una cadena?

La conversión de una fecha en una cadena es una tarea común en la programación, ya que permite mostrar la fecha de una manera más legible para los usuarios. También es útil en la creación de registros o informes que requieren una fecha en formato de texto. En este artículo, aprenderemos cómo convertir una fecha en una cadena utilizando C++.

## Cómo hacerlo

Primero, debemos incluir la biblioteca <string> y <ctime> en nuestro código para poder utilizar las funciones necesarias para convertir la fecha. Luego, podemos usar la función `strftime()` para formatear nuestra fecha en una cadena con el formato deseado.

```C++
#include <string>
#include <ctime>

int main() {
  time_t now = time(0);
  struct tm tstruct;
  char buffer[80];
  tstruct = *localtime(&now);

  // Formatear la fecha como "DD/MM/AAAA" 
  strftime(buffer, sizeof(buffer), "%d/%m/%Y", &tstruct);

  // Imprimir la fecha formateada
  std::cout << buffer << std::endl;

  return 0;
}
```

**Salida:**

10/05/2021

Podemos ver que la fecha se convirtió correctamente en una cadena con el formato deseado. También podemos cambiar el formato del buffer para obtener una cadena en diferentes estilos, como "AAAA-MM-DD" o "H:M:S". Para más información sobre los códigos de formato, puede consultar la documentación de la función `strftime()`.

## Profundizando

La función `strftime()` toma diferentes argumentos para controlar el formato de la cadena de fecha resultante. Por ejemplo, podemos especificar una fecha y hora específicas para formatear en lugar de usar la fecha actual. También podemos utilizar distintos códigos de formato para obtener información más específica sobre la fecha, como el nombre del mes o el día de la semana.

Además, es importante tener en cuenta que la función `strftime()` depende del idioma y la configuración regional del sistema. Esto significa que el formato de fecha puede variar según el idioma utilizado en el sistema.

## Ver también

- [Documentación de strftime() en cplusplus.com](https://www.cplusplus.com/reference/ctime/strftime/) 
- [Cómo trabajar con fechas en C++](https://www.geeksforgeeks.org/workin-with-dates-in-cpp/)