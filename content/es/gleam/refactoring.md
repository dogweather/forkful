---
title:                "Refactorización"
date:                  2024-01-26T01:18:03.078024-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactorización"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/refactoring.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
La refactorización es el proceso de rehacer tu código para hacerlo más limpio, más mantenible, sin alterar su comportamiento externo. Los programadores refactorizan para mejorar la legibilidad, reducir la complejidad y hacer que el código sea más amigable para futuras actualizaciones o añadidos de características.

## Cómo hacerlo:
Digamos que tienes un fragmento de código donde estás haciendo algunos cálculos repetidos o manipulaciones de cadenas a través de múltiples funciones. Ese es un objetivo principal para la refactorización. Aquí tienes un antes y después usando Gleam, que pone un fuerte énfasis en la seguridad de tipos e inmutabilidad:

```gleam
// Antes de la refactorización
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("El área es \(area)")
}

// Después de la refactorización
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("El área es \(area)")
}

// En otra parte de tu código, llamarás a print_area así:
print_area(calculate_area(10, 20))
```

Ejemplo de salida:
```
El área es 200
```

Con la refactorización, hemos hecho que `print_area` se centre más en imprimir, mientras que el cálculo se maneja en otro lugar, haciendo que el código sea más modular y más fácil de reutilizar o probar.

## Análisis Profundo
La refactorización, como concepto, ha existido tanto como la programación misma; revisitar y limpiar el código es parte del buen mantenimiento. La formalización moderna de la refactorización, junto con muchas de las técnicas y patrones utilizados hoy en día, se pueden rastrear hasta el libro seminal de Martin Fowler "Refactoring: Improving the Design of Existing Code" publicado en 1999.

En el ecosistema de Gleam, la refactorización tiene consideraciones específicas. Una de las más significativas es la fuerte comprobación de tipos en tiempo de compilación, que puede ayudar a detectar errores temprano cuando estás moviendo cosas. Las características de coincidencia de patrones e inmutabilidad de Gleam también pueden guiarte a escribir un código más claro y conciso, uno de los objetivos primarios de la refactorización.

Las alternativas a la refactorización podrían incluir reescribir el código desde cero o parchear el código con soluciones rápidas. Sin embargo, la refactorización suele ser el enfoque más seguro y más eficiente para mejorar el código existente sin introducir nuevos errores, ya que implica transformaciones incrementales, bien subrayadas, que preservan el comportamiento.

## Ver También
- El libro "Refactoring" de Martin Fowler: https://martinfowler.com/books/refactoring.html
- El sitio web de Gleam, con documentación adicional y ejemplos: https://gleam.run/
- "Refactoring: Improving the Design of Existing Code" de Martin Fowler (para principios subyacentes aplicables a través de lenguajes): https://martinfowler.com/books/refactoring.html
