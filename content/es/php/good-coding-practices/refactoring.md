---
title:                "Refactorización"
aliases:
- /es/php/refactoring.md
date:                  2024-01-26T01:47:49.972137-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactorización"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/refactoring.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
La refactorización es el proceso de reestructurar el código de un ordenador existente sin cambiar su comportamiento externo. Los programadores refactorizan para mejorar atributos no funcionales del software, haciendo el código más limpio, más eficiente y más fácil de mantener.

## Cómo hacerlo:
Tomemos un fragmento clásico de PHP y apliquemos un poco de magia de refactorización.

Antes de refactorizar, nuestro código podría verse así:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Artículo: " . $item['name'];
        echo " - Precio: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Total: " . array_sum(array_column($order, 'price'));
    }
}
```

Pero podemos refactorizar este código para mejorar su claridad y modularidad:

```php
function printItem($item) {
    echo "Artículo: {$item['name']} - Precio: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Total: " . calculateTotal($order);
    }
}
```
Al descomponer la función `printOrderDetails` en funciones más pequeñas, nuestro código se vuelve más legible y fácil de depurar.

## Un Vistazo Profundo
La refactorización tiene sus raíces en la comunidad de programación de Smalltalk a principios de los años 90 y fue popularizada aún más por el libro seminal de Martin Fowler "Refactoring: Improving the Design of Existing Code" (1999). Aunque la refactorización se puede aplicar a cualquier lenguaje de programación, la naturaleza dinámica de PHP permite algunos desafíos y oportunidades únicos.

Las alternativas a la refactorización podrían incluir reescribir el código desde cero, lo cual a menudo es más arriesgado y lleva más tiempo. En el ecosistema de PHP, herramientas como PHPStan y Rector pueden detectar y realizar algunas operaciones de refactorización automáticamente, respectivamente. En términos de implementación, mantener las refactorizaciones pequeñas y probar extensivamente con pruebas unitarias son prácticas clave para asegurar una refactorización exitosa sin introducir errores.

## Ver También
- Libro de Refactorización de Martin Fowler: https://martinfowler.com/books/refactoring.html
- PHPStan, una herramienta de análisis estático de PHP: https://phpstan.org/
- Rector, una herramienta para la refactorización automática del código PHP: https://getrector.org/
- Pruebas de Unidad en PHP con PHPUnit: https://phpunit.de/
