---
date: 2024-01-26 03:43:00.718458-07:00
description: "Arredondar n\xFAmeros \xE9 cortar um decimal para o seu valor inteiro\
  \ mais pr\xF3ximo ou para um n\xFAmero determinado de casas decimais. Os programadores\
  \ arredondam\u2026"
lastmod: '2024-03-13T22:44:46.834350-06:00'
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros \xE9 cortar um decimal para o seu valor inteiro mais\
  \ pr\xF3ximo ou para um n\xFAmero determinado de casas decimais. Os programadores\
  \ arredondam\u2026"
title: "Arredondamento de n\xFAmeros"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Arredondar números é cortar um decimal para o seu valor inteiro mais próximo ou para um número determinado de casas decimais. Os programadores arredondam números para torná-los mais fáceis de ler e manusear, especialmente quando a precisão além de um certo ponto é desnecessária ou poderia levar a erros.

## Como Fazer:
No Arduino, você pode arredondar números usando funções embutidas. Os principais jogadores são `round`, `ceil` e `floor`. Aqui está uma demonstração rápida:

```arduino
void setup() {
  Serial.begin(9600);
  
  float myNumber = 123.4567;

  // Arredonda para o número inteiro mais próximo
  Serial.println(round(myNumber)); // Saída: 123

  // Sempre arredonda para cima
  Serial.println(ceil(myNumber));  // Saída: 124

  // Sempre arredonda para baixo
  Serial.println(floor(myNumber)); // Saída: 123
}

void loop() {
  // Nada para percorrer no loop.
}
```

## Mergulho Profundo:
Algoritmos de arredondamento têm uma longa história; eles existem muito antes dos computadores digitais. Em computação analógica, o arredondamento era um processo físico. Em computação digital, é um processo matemático.

O arredondamento é necessário quando convertemos de um tipo com mais precisão (como `float` ou `double`) para um tipo com menos precisão (como `int`). Mas a forma como arredondamos pode variar:

1. `round()`: Arredondamento padrão. Se a fração for 0,5 ou maior, sobe; caso contrário, desce.
2. `ceil()`: Abreviação de "teto", sempre arredonda para cima para o número inteiro mais próximo, mesmo que esteja mais próximo do número inferior.
3. `floor()`: Oposto de teto; sempre arredonda para baixo.

A escolha entre essas funções depende do propósito do valor arredondado. Medidas podem precisar de um arredondamento padrão, dinheiro muitas vezes usa `floor`, enquanto sistemas de inventário podem usar `ceil` para garantir que tudo esteja contabilizado.

A implementação dessas funções pelo Arduino é simples; elas não lidam com casos extras como arredondamento para casas decimais específicas. Para isso, uma função personalizada ou matemática mais profunda entra em jogo — pense em multiplicar para deslocar a decimal, arredondar e depois dividir de volta.

Os erros de arredondamento podem se acumular, impactando significativamente em cálculos longos ou processos iterativos. Os programadores precisam ser cautelosos ao executar numerosas operações em valores arredondados.

## Veja Também:
2. Um olhar aprofundado sobre as armadilhas e estratégias para arredondamento: [Guia de Ponto Flutuante](https://floating-point-gui.de/)
3. Para técnicas avançadas, incluindo funções de arredondamento personalizadas e tratamento de erro de arredondamento, você pode consultar recursos acadêmicos ou guias de programação detalhados.
