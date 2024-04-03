---
date: 2024-01-26 04:37:05.609660-07:00
description: 'Como fazer: .'
lastmod: '2024-03-13T22:44:46.833388-06:00'
model: gpt-4-0125-preview
summary: .
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## Como fazer:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // Inicia a comunicação serial
  
  Complex myComplex(2, 3); // Cria um número complexo 2 + 3i
  Complex anotherComplex(1, 1); // Cria outro número complexo 1 + 1i
  
  // Adição
  Complex result = myComplex + anotherComplex; 
  Serial.print("Adição: "); 
  result.print(); // Saídas 3 + 4i
  
  // Multiplicação
  result = myComplex * anotherComplex; 
  Serial.print("Multiplicação: ");
  result.print(); // Saídas -1 + 5i
}

void loop() {
  // Não usado neste exemplo
}
```
Saída do exemplo:
```
Adição: 3 + 4i
Multiplicação: -1 + 5i
```

## Aprofundando
Originalmente, os números complexos foram recebidos com ceticismo, mas eles se tornaram centrais em diversos campos científicos. Historicamente, eles foram reconhecidos por fornecer soluções para equações polinomiais que não possuem soluções reais.

O Arduino não inclui números complexos em sua biblioteca padrão, mas você pode aproveitar bibliotecas como `Complex.h` para manuseá-los. Internamente, essas bibliotecas definem uma Classe Complexa, tipicamente usando dois doubles para armazenar as partes real e imaginária, e sobrecarregam operadores para suportar aritmética.

Como alternativa, para aplicações que não precisam inerentemente da aritmética de números complexos, considere usar outras estratégias ou bibliotecas matemáticas. Lembre-se, porém, que usar floats em vez de números complexos poderia simplificar demais alguns problemas.

## Veja Também
- A biblioteca [Complex.h](https://github.com/RobTillaart/Complex) por Rob Tillaart.
- Um mergulho mais profundo na [matemática por trás dos números complexos](https://mathworld.wolfram.com/ComplexNumber.html).
