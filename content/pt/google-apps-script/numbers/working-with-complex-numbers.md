---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:33.754844-07:00
description: "Como fazer: O Google Apps Script n\xE3o tem suporte integrado para n\xFA\
  meros complexos, o que necessita da implementa\xE7\xE3o de funcionalidades personalizadas.\u2026"
lastmod: '2024-03-13T22:44:46.100481-06:00'
model: gpt-4-0125-preview
summary: "O Google Apps Script n\xE3o tem suporte integrado para n\xFAmeros complexos,\
  \ o que necessita da implementa\xE7\xE3o de funcionalidades personalizadas."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## Como fazer:
O Google Apps Script não tem suporte integrado para números complexos, o que necessita da implementação de funcionalidades personalizadas. Abaixo está uma estrutura básica para lidar com números complexos, incluindo adição, subtração e multiplicação.

```javascript
// Define um construtor para números complexos
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// Método para adicionar dois números complexos
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// Método para subtrair dois números complexos
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// Método para multiplicar dois números complexos
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// Exemplo de uso
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// Adicionar dois números complexos
var soma = num1.add(num2);
console.log(`Soma: ${soma.real} + ${soma.imag}i`); // Soma: 4 + 6i

// Subtrair dois números complexos
var diferenca = num1.subtract(num2);
console.log(`Diferença: ${diferenca.real} + ${diferenca.imag}i`); // Diferença: 2 + 2i

// Multiplicar dois números complexos
var produto = num1.multiply(num2);
console.log(`Produto: ${produto.real} + ${produto.imag}i`); // Produto: -5 + 10i
```

## Mais Detalhes:
O conceito de números complexos remonta ao século 16, mas foi o trabalho de matemáticos como Euler e Gauss que solidificou seu lugar na matemática. Apesar de sua utilidade, os números complexos não são diretamente suportados em JavaScript ou, por extensão, no Google Apps Script. A falta de suporte nativo significa que as operações em números complexos têm que ser implementadas manualmente, como demonstrado. Embora isso proporcione uma boa oportunidade de aprendizado e funcionalidade suficiente para necessidades básicas, para trabalhos computacionais pesados que requerem números complexos, pode-se considerar o uso de outros ambientes de programação mais adequados para a computação matemática, como Python com NumPy, que oferecem operações integradas e altamente otimizadas para lidar com números complexos. Contudo, compreender e implementar operações básicas no Google Apps Script é um exercício útil para aqueles que procuram ampliar suas habilidades de programação e aplicá-las em uma ampla gama de contextos.
