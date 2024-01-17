---
title:                "Comparando dos fechas"
html_title:           "Arduino: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

"¿Qué y Por qué?": Comparar dos fechas es cuando un programador quiere averiguar si una fecha es anterior o posterior a otra. Esto se usa a menudo en programas que requieren una lógica basada en el tiempo, como en un reloj o un cronómetro. Es una forma de calcular el paso del tiempo en un programa para tomar decisiones o realizar acciones específicas.

"## Cómo hacerlo:"

Para comparar dos fechas en Arduino, se pueden seguir los siguientes pasos:

1. Definir dos variables de tipo ```String``` que representen las fechas a comparar. Por ejemplo:
```
String fecha1 = "2021-01-01";
String fecha2 = "2021-02-01";
```

2. Utilizar la función ```strcmp()``` para comparar las dos fechas. Esta función compara dos cadenas de texto y devuelve un valor numérico dependiendo del resultado de la comparación. En nuestro caso, si la primera fecha es anterior a la segunda, el valor devuelto será menor que cero. Si son iguales, el valor será igual a cero. Y si la primera fecha es posterior a la segunda, el valor será mayor que cero. Ejemplo de código:
```
int resultado = strcmp(fecha1.c_str(), fecha2.c_str());
```

3. Utilizar una estructura condicional para tomar decisiones basadas en el resultado de la comparación. Por ejemplo, si la primera fecha es anterior a la segunda, imprimir un mensaje en la pantalla LCD. Ejemplo de código:
```
if (resultado < 0) {
    // Imprimir mensaje en pantalla LCD
}
```

"## Profundizando:"

En la programación, las fechas se representan a menudo como cadenas de texto en un formato específico, como el usado en el ejemplo (año-mes-día). Esto permite una fácil comparación utilizando la función ```strcmp()```. Sin embargo, también existen otras formas de trabajar con fechas en Arduino, como utilizando la librería Time.

Otra manera de comparar fechas es utilizando operadores lógicos, como los operadores de comparación (>, <, ==) o los operadores lógicos (&&, ||). Sin embargo, requiere más líneas de código y no es tan eficiente como utilizar la función ```strcmp()```.

Es importante tener en cuenta que al comparar fechas, se deben tener en cuenta factores como los años bisiestos y la diferencia entre meses con diferente cantidad de días.

"## Ver también:"

- Documentación oficial de Arduino sobre la función ```strcmp()```: https://www.arduino.cc/reference/es/language/variables/comparison/strcmp/
- Librería Time para trabajar con fechas y tiempos en Arduino: https://www.pjrc.com/teensy/td_libs_Time.html
- Tutorial sobre operadores lógicos en Arduino: https://learn.sparkfun.com/tutorials/arduino-operadores-logicos-y-de-comparacion/all