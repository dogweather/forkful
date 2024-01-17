---
title:                "Escribiendo pruebas"
html_title:           "PHP: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Es importante escribir pruebas en tu código de PHP para asegurarse de que funciona correctamente. Las pruebas son una forma de verificar si tu código hace lo que se supone que debe hacer. Los programadores las escriben para garantizar la calidad de su código y reducir el riesgo de errores en el futuro.

## Cómo:
Aquí te mostraremos cómo escribir pruebas en tu código de PHP utilizando el marco de pruebas PHPUnit. Primero, es necesario instalar PHPUnit en tu proyecto a través de Composer. Luego, crea una carpeta llamada "pruebas" en tu directorio de proyecto y crea un archivo de prueba llamado "Prueba.php". Luego, escribe las siguientes líneas de código dentro del archivo:

```
<?php
use PHPUnit\Framework\TestCase;

class Prueba extends TestCase {
  public function testSuma() {
    $resultado = 2 + 3;
    $this->assertEquals(5, $resultado);
  }
}
```

Este código crea una clase de prueba que hereda de la clase TestCase de PHPUnit. El método "testSuma()" se encarga de realizar la prueba: suma 2 y 3 y comprueba si el resultado es igual a 5 utilizando el método "assertEquals()". Para ejecutar esta prueba, desde la terminal, ejecuta el siguiente comando en tu directorio de proyecto:

```
vendor/bin/phpunit pruebas
```

Si la prueba pasa, el resultado será algo como esto:

```
OK (1 prueba, 1 assertion)
```

## Deep Dive:
Ahora que sabes cómo escribir pruebas en PHP, es importante entender por qué deberías hacerlo y cuáles son algunas alternativas. Antes, los programadores solían realizar pruebas manuales, que eran tediosas y propensas a errores humanos. PHPUnit fue uno de los primeros marcos de pruebas desarrollados específicamente para pruebas de PHP. Sin embargo, también existen otras opciones como Behat o Codeception que utilizan pruebas basadas en comportamiento en lugar de por funciones.

Para implementar pruebas en proyectos más grandes, también se pueden utilizar herramientas de integración continua como Jenkins o Travis CI. Estas herramientas te permiten automatizar la ejecución de pruebas para cada cambio en tu código.

## See Also:
- Documentación de pruebas PHPUnit: https://phpunit.de/manual/current/en/
- Alternativas a PHPUnit: https://www.keycdn.com/blog/php-testing-tools
- Tutorial de integración continua con Jenkins en PHP: https://www.toptal.com/php/integration-continuous-deployment-php-jenkins