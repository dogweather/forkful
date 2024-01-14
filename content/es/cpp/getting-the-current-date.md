---
title:    "C++: Obteniendo la fecha actual"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Una de las tareas más comunes a la hora de programar es obtener la fecha actual. Ya sea para generar informes, establecer fechas de vencimiento o simplemente mostrar la fecha actual en una aplicación, es fundamental saber cómo obtener la fecha en C++. En esta publicación, te explicaremos por qué es importante y cómo hacerlo de manera sencilla.

## Cómo hacerlo

Para obtener la fecha actual en C++, podemos utilizar la biblioteca estándar `ctime`. Esta biblioteca incluye la función `time`, que nos permite obtener la hora actual en forma de un objeto de tipo `time_t`. A continuación, podemos utilizar la función `strftime` para formatear el objeto `time_t` en el formato de fecha deseado.

Veamos un ejemplo de cómo obtener la fecha actual y mostrarla en formato "día/mes/año":

```C++
#include <iostream>
#include <ctime>

int main() {
  // Obtenemos la hora actual
  time_t now = time(0);
  
  // Creamos un objeto de tipo tm para almacenar la fecha
  struct tm *date = localtime(&now);
  
  // Utilizamos strftime para formatear la fecha
  char buffer[9];
  strftime(buffer, 9, "%d/%m/%Y", date);
  
  // Imprimimos la fecha formateada
  std::cout << "Hoy es " << buffer << std::endl;
  
  return 0;
}
```

La salida de este programa sería "Hoy es 14/08/2021".

## Deep Dive

La función `time` de la biblioteca `ctime` retorna un valor de tipo `time_t`, que es en realidad un entero que representa el número de segundos transcurridos desde el 1 de enero de 1970 a las 00:00 UTC. Esto se conoce como el "epoch" de UNIX.

El formato en el que se muestra la fecha depende de cómo lo especificamos en la función `strftime`. En nuestro ejemplo, utilizamos "%d" para el día, "%m" para el mes y "%Y" para el año. Puedes encontrar una lista completa de los códigos de formato aquí: [https://www.cplusplus.com/reference/ctime/strftime/](https://www.cplusplus.com/reference/ctime/strftime/)

También es importante mencionar que la función `localtime` convierte el objeto `time_t` en la zona horaria local. Si deseas obtener la fecha en una zona horaria diferente, puedes utilizar la función `gmtime`.

## Ver también

- [https://www.cplusplus.com/reference/ctime/](https://www.cplusplus.com/reference/ctime/)
- [https://en.cppreference.com/w/cpp/chrono/c/strftime](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [https://www.cplusplus.com/forum/beginner/234327/](https://www.cplusplus.com/forum/beginner/234327/) (enlaces en inglés)