---
title:                "Refactorización"
aliases:
- /es/ruby/refactoring/
date:                  2024-01-26T03:36:43.272948-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactorización"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/refactoring.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La refactorización es el proceso de reestructurar el código de computadora existente sin cambiar su comportamiento externo. Los programadores refactorizan para mejorar atributos no funcionales del software, como la legibilidad, reducción de complejidad, mejora en la mantenibilidad o el aumento del rendimiento.

## Cómo hacerlo:

Vamos a seguir el ejemplo de la refactorización de un método de Ruby que calcula la suma de cuadrados.

**Antes de la Refactorización:**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # Salida: 14
```

**Después de la Refactorización:**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # Salida: 14
```

La versión refactorizada usa Enumerables de Ruby para expresar la misma lógica de manera más sucinta y clara. El método `map` transforma cada elemento y `sum` agrega sus valores, eliminando la necesidad de gestionar bucles manualmente y la asignación de variables.

## Análisis Profundo

La refactorización tiene un rico contexto histórico, que se remonta a las prácticas tempranas en el desarrollo de software. Las primeras menciones se pueden rastrear hasta la década de 1990, con contribuciones significativas hechas por Martin Fowler en su libro "Refactoring: Improving the Design of Existing Code", donde proporciona un catálogo de patrones para la refactorización. Desde entonces, la refactorización se ha convertido en un pilar de las prácticas de desarrollo ágil.

Cuando hablamos de alternativas a la refactorización, necesitamos considerar un enfoque diferente como 'Reescribir', donde reemplazas el sistema antiguo en partes o en su totalidad, o adaptar prácticas como 'Revisiones de Código' y 'Programación en Pareja' para mejorar la calidad del código gradualmente. Sin embargo, estas no son reemplazos para la refactorización; complementan el proceso.

En términos de implementación, Ruby proporciona una sintaxis excelente y expresiva que a menudo resulta en código más corto y legible después de la refactorización. Los principios clave incluyen DRY (No Te Repitas), usar nombres significativos, mantener los métodos cortos y enfocados en una sola tarea, y usar efectivamente el módulo Enumerable de Ruby, como se ve en el ejemplo anterior. Herramientas automatizadas como RuboCop también pueden ayudar a los programadores a identificar partes en el código que podrían beneficiarse de la refactorización.

## Ver También

Para profundizar más en la refactorización en Ruby, consulta estos recursos:

- El libro seminal de Martin Fowler: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- La guía de estilo de Ruby para escribir código más limpio: [The Ruby Style Guide](https://rubystyle.guide/)
- RuboCop, un analizador de código estático (lint) y formateador: [Repositorio GitHub de RuboCop](https://github.com/rubocop/rubocop)
