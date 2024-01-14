---
title:    "C++: Calculando una fecha en el futuro o pasado."
keywords: ["C++"]
---

{{< edit_this_page >}}

## ¿Por qué?

Calcular una fecha en el futuro o en el pasado es una habilidad útil para cualquier programador de C++. Puede ser usado para tareas simples como crear un calendario de eventos o para aplicaciones más complejas que requieren cálculos de fechas precisos.

## ¿Cómo hacerlo?

Para calcular una fecha en el futuro o en el pasado en C++, primero necesitamos usar la biblioteca de fechas y tiempos estándar de C++ (```<chrono>``` y ```<iomanip>```). Luego, necesitamos definir una fecha inicial y una duración de tiempo en días, meses o años.

```C++
#include <iostream>
#include <chrono>
#include <iomanip>

int main(){
    // Define fecha inicial
    std::chrono::system_clock::time_point today = std::chrono::system_clock::now();
    std::time_t ttime = std::chrono::system_clock::to_time_t(today);
    // Duration para agregar a la fecha inicial en días, meses o años
    std::chrono::duration<int, std::ratio<3600*24>> duration(365);
    
    // Calcular fecha en el futuro
    std::cout << "Fecha en el futuro:" << std::endl;
    std::cout << std::put_time(std::localtime(&ttime), "%F") << std::endl;
    ttime = std::chrono::system_clock::to_time_t(today + duration);
    std::cout << "Fecha en el futuro + 1 año:" << std::endl;
    std::cout << std::put_time(std::localtime(&ttime), "%F") << std::endl;
    
    // Calcular fecha en el pasado
    std::cout << "Fecha en el pasado:" << std::endl;
    std::cout << std::put_time(std::localtime(&ttime), "%F") << std::endl;
    ttime = std::chrono::system_clock::to_time_t(today - duration);
    std::cout << "Fecha en el pasado - 1 año:" << std::endl;
    std::cout << std::put_time(std::localtime(&ttime), "%F") << std::endl;
    
    return 0;
}
```

**Salida:**

```
Fecha en el futuro:
2021-08-28
Fecha en el futuro + 1 año:
2022-08-28
Fecha en el pasado:
2022-08-28
Fecha en el pasado - 1 año:
2021-08-28
```

## Profundizando

Cuando trabajamos con fechas y tiempos en C++, es importante tener en cuenta que hay diferentes tipos de representación de tiempo (como local time, UTC time, etc.) y podemos utilizar diferentes métodos para convertir entre ellos. También es importante considerar cuestiones relacionadas con la precisión del cálculo, ya que hay factores como los años bisiestos que pueden afectar los resultados.

En general, al calcular una fecha en el futuro o en el pasado en C++, es importante ser conscientes de estas cuestiones y elegir los métodos y herramientas adecuados para lograr el resultado deseado.

## Ver también

- Documentación oficial de C++ sobre la Biblioteca de Fechas y Tiempos: https://en.cppreference.com/w/cpp/chrono
- Tutorial sobre fechas y tiempos en C++: https://www.learncpp.com/c-tutorial/time-date-and-timing/
- Ejemplos de cálculos de fechas con diferentes preciones en C++: https://www.techiedelight.com/calculate-date-time-difference-cpp/