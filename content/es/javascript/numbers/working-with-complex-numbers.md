---
date: 2024-01-26 04:42:01.243846-07:00
description: "Los n\xFAmeros complejos son n\xFAmeros con una parte real y una parte\
  \ imaginaria (como 3 + 4i). Aparecen en varios problemas de programaci\xF3n, especialmente\
  \ en\u2026"
lastmod: '2024-02-25T18:49:55.922347-07:00'
model: gpt-4-0125-preview
summary: "Los n\xFAmeros complejos son n\xFAmeros con una parte real y una parte imaginaria\
  \ (como 3 + 4i). Aparecen en varios problemas de programaci\xF3n, especialmente\
  \ en\u2026"
title: "Trabajando con n\xFAmeros complejos"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Los números complejos son números con una parte real y una parte imaginaria (como 3 + 4i). Aparecen en varios problemas de programación, especialmente en el procesamiento de señales, la computación cuántica y la resolución de ecuaciones polinómicas. Los programadores los manejan para abordar eficazmente este tipo de tareas.

## Cómo hacerlo:
JavaScript no tiene soporte integrado para números complejos, pero puedes arremangarte y manejarlo con objetos y matemáticas. Aquí tienes un rápido vistazo.

```javascript
class NumeroComplejo {
  constructor(real, imaginario) {
    this.real = real;
    this.imaginario = imaginario;
  }

  add(otro) {
    return new NumeroComplejo(this.real + otro.real, this.imaginario + otro.imaginario);
  }

  // ...agrega más métodos (restar, multiplicar, dividir) según sea necesario

  toString() {
    return `${this.real} + ${this.imaginario}i`;
  }
}

const a = new NumeroComplejo(1, 2);
const b = new NumeroComplejo(3, 4);
const resultado = a.add(b);

console.log(`Resultado: ${resultado}`); // Resultado: 4 + 6i
```

## Profundización
Los números complejos existen desde el siglo XVI, gracias al matemático italiano Gerolamo Cardano. Se volvieron cruciales en varios campos, como la ingeniería y la física. En la programación moderna, son clave para simulaciones y algoritmos que necesitan multidimensionalidad.

Ahora, JavaScript no está preparado de forma nativa para los números complejos. Pero además de la opción de hacerlo tú mismo, podrías usar bibliotecas matemáticas como math.js o numeric.js. Aportan la potencia para un manejo más pesado de números complejos, agregando ventajas como más operaciones, cálculo de magnitud y búsqueda de argumentos.

Debajo del capó, cuando operas con números complejos, es como si manejaras dos números separados unidos a la cadera. La adición y la sustracción son juegos directos—empareja lo real con lo real, lo imaginario con lo imaginario. La multiplicación y la división se ponen picantes con cruces de términos y necesitan más cuidado.

## Ver también
- MDN Web Docs sobre JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, una biblioteca matemática que incluye números complejos: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, otra biblioteca: http://numericjs.com/documentation.html
- Una inmersión más profunda en los números complejos (enfoque matemático): https://mathworld.wolfram.com/ComplexNumber.html
