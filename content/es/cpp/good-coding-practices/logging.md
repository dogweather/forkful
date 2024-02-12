---
title:                "Registro de Actividades en Programación"
aliases:
- /es/cpp/logging/
date:                  2024-01-26T01:00:34.141394-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Actividades en Programación"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/logging.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
El registro (logging) en el contexto de la programación es el proceso de grabar eventos, estados e información en un archivo u otro medio de salida. Los programadores realizan registros para seguir la pista de lo que está sucediendo en sus aplicaciones, para depurar problemas y para monitorear el rendimiento para análisis y optimización futuros.

## Cómo hacerlo:
Digamos que estás trabajando en una máquina Linux y quieres volcar tus registros en un archivo usando el buen y viejo C++. Querrás incluir las librerías `<iostream>` y `<fstream>` para realizar operaciones con archivos. Aquí tienes un ejemplo rápido:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream logFile("appLog.txt", std::ios::app);  // Abrir en modo de añadir

    if (!logFile.is_open()) {
        std::cerr << "¡Hubo un problema al abrir el archivo de registro!" << std::endl;
        return 1;
    }

    logFile << "Aplicación iniciada" << std::endl;
  
    // ... en algún lugar de la lógica de tu aplicación
    logFile << "Ha ocurrido un evento importante" << std::endl;

    // No olvides cerrar tu flujo de archivo
    logFile.close();

    return 0;
}
```

Si observas tu archivo de registro con `tail -f appLog.txt`, deberías ver:

```
Aplicación iniciada
Ha ocurrido un evento importante
```

¡Genial, has conseguido un registro cronológico de eventos!

## Inmersión Profunda
El registro es tan antiguo como la computación misma, con raíces en marcas literales en papel para rastrear lo que los antiguos ordenadores estaban haciendo. En la era moderna, todo se trata de soluciones de software sofisticadas. Tienes registro directo a archivo, como el ejemplo rápido y sucio de arriba, o puedes complacerte con un marco de registro más elegante, como Log4cpp o Boost.Log en el ámbito de C++; estos chicos malos ofrecen niveles de registro, control de formato y más.

Hablando de niveles, las mejores prácticas de registro incluyen el uso de diversos niveles de severidad —información, depuración, advertencia, error, fatal— para que puedas filtrar el ruido cuando estás tratando de aplastar errores o averiguar por qué tu aplicación se comporta como un adolescente con cambios de humor.

En cuanto al rendimiento, no te descuides con tus registros. Un registro excesivo puede transformar tu aplicación rapidísima en un maratón de caracoles, sobrecargar sistemas de archivos o incluso costarte dinero en tarifas de almacenamiento si estás basado en la nube. Encontrar el equilibrio correcto es la clave: registra lo que necesitas y nada más.

## Ver También
Para aquellos de ustedes que les gusta ir más allá con sus prácticas de registro, echen un vistazo a:

- La [Biblioteca Boost.Log](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html) para algunas características de registro de gran alcance.
- [La biblioteca glog de Google](https://github.com/google/glog) si te interesa lo que los cocineros del gigante tecnológico están usando para registrar sus aplicaciones.
- [La biblioteca Log4cpp](http://log4cpp.sourceforge.net/) para un mecanismo de registro configurable.

Y para un poco de lectura de fondo sobre los porqués y cómos del registro, sumérgete en:

- Este hilo de Stack Overflow sobre [las mejores prácticas de registro](https://stackoverflow.com/questions/783956/logging-best-practices) que te dará una inmersión profunda revisada por pares en el tema.
