---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Arduino: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una fecha en una cadena es el proceso de convertir una fecha en un formato legible para los seres humanos, como "12 de agosto de 2021", en lugar de un valor numérico como "12-08-2021". Los programadores hacen esto para mejorar la legibilidad y comprensión de la fecha en sus programas.

## Cómo hacerlo:

```Arduino
// Ejemplo de código para convertir la fecha actual en una cadena
void setup(){
    Serial.begin(9600); // Iniciar la comunicación con el monitor serial
}

void loop(){
    int dia = day(); // Obtener el día actual
    int mes = month(); // Obtener el mes actual
    int año = year(); // Obtener el año actual

    // Convertir los valores numéricos en una cadena
    String fecha = String(dia) + "-" + String(mes) + "-" + String(año);
    
    // Imprimir la fecha en el monitor serial
    Serial.println("La fecha actual es: " + fecha);
    delay(1000); // Esperar un segundo antes de repetir el ciclo
}
```

El código anterior utiliza la función `String()` para convertir los valores numéricos en cadenas y luego los concatena para formar la fecha completa. Esta es una forma sencilla de convertir una fecha en una cadena, pero también puedes utilizar bibliotecas o funciones personalizadas para obtener resultados más precisos.

## Viaje profundo:

### Contexto histórico
Antes de la programación moderna, las fechas se almacenaban como valores numéricos y los programadores tenían que recordar el formato correcto para cada país o región en particular. Con la evolución de la programación orientada a objetos, surgió la necesidad de convertir fechas en cadenas legibles, lo que llevó al desarrollo de diferentes métodos y funciones para lograr este objetivo.

### Alternativas
Además de usar la función `String()`, puedes utilizar bibliotecas especializadas para convertir fechas en cadenas, como la biblioteca `Time` que viene incluida en la instalación de Arduino. También existen funciones personalizadas y algoritmos para este propósito en línea, pero debes asegurarte de entender su funcionamiento antes de implementarlos en tu código.

### Detalles de implementación
La función `String()` en Arduino es una sobrecarga del constructor predeterminado de la clase `String`. Toma diferentes tipos de datos como parámetros y los convierte en cadenas utilizando una serie de pasos internos. Sin embargo, debes tener en cuenta que cada vez que utilizas la función `String()`, se reserva memoria adicional para almacenar la cadena, lo que puede ser un problema en proyectos con limitaciones de memoria.

## Ver también:

- Documentación oficial de la función `String()` en [Arduino Reference](https://www.arduino.cc/reference/en/language/functions/communication/string/)
- Uso avanzado de la conversión de fechas en cadenas en [Instructables](https://www.instructables.com/Date-to-String-Arduino/)
- Ejemplos prácticos de la biblioteca `Time` en [Random Nerd Tutorials](https://randomnerdtutorials.com/date-and-time-using-arduino-and-ds3231-real-time-clock-module/)