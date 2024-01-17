---
title:                "Escribiendo pruebas"
html_title:           "Gleam: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-tests.md"
---

{{< edit_this_page >}}

¿Qué es y por qué se hace la escritura de pruebas?
Escribir pruebas es un proceso en el que los programadores escriben código adicional para verificar si sus programas funcionan correctamente. Este proceso se realiza para garantizar que los cambios o modificaciones en el código no rompan la funcionalidad existente. Los programadores hacen esto para asegurarse de que sus programas son confiables y funcionan correctamente.

Cómo hacerlo:
```Gleam
//Escribiendo una prueba básica
test "Suma de dos números" {
  assert.equal(2 + 2, 4)
}
```

```Gleam
//Ejemplo de una prueba que falla
test "División por cero" {
  assert.equal(10 / 0, 5)
}
//Salida: La prueba falló debido a una división por cero.
```

```Gleam
//Uso de "assert.not_equal" para probar si dos valores no son iguales
test "Valores no iguales" {
  assert.not_equal(3, 4)
}
```

Más información:
La escritura de pruebas se ha vuelto cada vez más importante en la programación moderna debido al aumento de la complejidad del código. Otro enfoque para garantizar la calidad del código es la revisión manual por parte de otros programadores, pero esto puede ser muy lento y propenso a errores. La escritura de pruebas automatizadas ayuda a detectar cualquier problema en el código de manera rápida y eficiente.

Véase también:
- Documentación oficial de Gleam sobre escritura de pruebas: https://gleam.run/book/testing.html
- Pruebas unitarias vs pruebas de integración: https://blog.testlodge.com/unit-testing-vs-integration-testing/
- Principales marcos de prueba para programadores de Gleam: https://medium.com/@taylorwrogers/7-testing-frameworks-for-gleam-programmers-6099f5b10542