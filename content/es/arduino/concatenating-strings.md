---
title:    "Arduino: Uniendo cadenas"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar concatenación de cadenas en Arduino?

La concatenación de cadenas es una técnica muy útil en programación que permite unir varias cadenas de texto en una sola. Esto es especialmente útil en Arduino, ya que se pueden crear mensajes más complejos que pueden ser enviados a través de la comunicación serial o mostrados en una pantalla LCD.

## Cómo hacerlo en Arduino

Para concatenar cadenas en Arduino, se utiliza la función `concat()` junto con el operador `+=`. En el siguiente ejemplo, se unen dos cadenas y el resultado se guarda en una tercera variable:

```
Arduino string uno = "Hola ";
Arduino string dos = "mundo";
Arduino string tres = uno;
tres += dos;
```

El resultado sería una nueva cadena `Hola mundo` almacenada en la variable `tres`. También es posible concatenar más de dos cadenas a la vez y se pueden utilizar variables en lugar de cadenas fijas.

## Profundizando en la concatenación de cadenas

La concatenación de cadenas puede ser especialmente útil cuando se combinan con otras técnicas de programación como bucles y condicionales. También es importante tener en cuenta que, debido a la limitada cantidad de memoria en Arduino, es importante optimizar el uso de concatenación de cadenas para evitar agotar la memoria disponible.

## Ver también

- [Documentación oficial de Arduino sobre concatenación de cadenas](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [Ejemplo de concatenación de cadenas en Arduino](https://www.hackster.io/Arduino_Genuino/strings-d2101f)