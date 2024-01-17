---
title:                "Obtener la fecha actual."
html_title:           "PHP: Obtener la fecha actual."
simple_title:         "Obtener la fecha actual."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

¿Qué es y por qué los programadores utilizan la función para obtener la fecha actual en PHP?

La función para obtener la fecha actual en PHP se utiliza para obtener la fecha y hora actuales del servidor en el que se ejecuta el código PHP. Es una función útil para mostrar la fecha y hora actual en una página web o generar archivos con fecha y hora específicas.

Cómo utilizar la función para obtener la fecha actual en PHP:
```PHP
<?php
    // Utilizamos la función date() para obtener la fecha y hora actuales en el formato deseado
    $fecha_actual = date('d/m/Y H:i:s'); 
    // Imprimimos la fecha actual en la página web o la utilizamos en nuestro código
    echo "La fecha y hora actual es: ".$fecha_actual;
?>
```
**Resultado:**
```
La fecha y hora actual es: 05/06/2021 13:45:20
```

Más detalles sobre la función:

- Contexto histórico:
La función para obtener la fecha actual en PHP fue introducida en la versión 4.0 de PHP y ha estado disponible desde entonces. Anteriormente, los programadores tenían que utilizar diferentes funciones y métodos para obtener la fecha y hora actuales.

- Otras alternativas:
Además de la función date(), también se pueden utilizar otras funciones como time() para obtener la fecha y hora en formato UNIX, pero se requeriría un formato de conversión posterior. También se pueden utilizar librerías externas como Carbon para manejar fechas y horas de una manera más avanzada.

- Detalles de implementación:
La función date() toma dos argumentos opcionales, el primero es el formato de fecha y hora deseado y el segundo es un timestamp opcional que permite obtener la fecha y hora en un momento específico. Además, se pueden utilizar diferentes modificadores en el formato de fecha para obtener diferentes resultados.

Ver también:
- Documentación oficial de PHP sobre la función date(): [https://www.php.net/manual/es/function.date.php](https://www.php.net/manual/es/function.date.php)
- Documentación oficial de PHP sobre la función time(): [https://www.php.net/manual/es/function.time.php](https://www.php.net/manual/es/function.time.php)
- Documentación oficial de Carbon: [https://carbon.nesbot.com/docs/](https://carbon.nesbot.com/docs/)