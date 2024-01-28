---
title:                "Arredondamento de números"
date:                  2024-01-26T03:45:46.453990-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arredondamento de números"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Arredondar números é ajustar um valor para o lugar especificado mais próximo—como de 2,56 para 3, se estivermos arredondando para números inteiros. Programadores fazem isso por simplicidade ou para atender a certas especificações numéricas, geralmente para evitar nuances causadas por erros de precisão em ponto flutuante ou para tornar a saída mais amigável ao usuário.

## Como fazer:
No Gleam, arredondar não está na biblioteca padrão até a minha última verificação, mas aqui está como você normalmente arredonda um float para o número inteiro mais próximo usando funções do Erlang diretamente:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Saídas: 3
}
```

Saída:
```
3
```

Tem uma precisão diferente em mente? Diz, arredondando para duas casas decimais? Precisamos de um pouco de matemática:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Saídas: 2.57
}
```

Saída:
```
2.57
```

## Aprofundamento
Historicamente, arredondar números tem sido crucial, especialmente em cálculos financeiros e científicos onde a precisão e padrões importam muito. Sem arredondamento, você obteria decimais longos e desagradáveis por toda parte, tornando os cálculos impraticáveis e propensos a erros.

No mundo da programação, diferentes linguagens oferecem abordagens distintas, desde funções embutidas até bibliotecas matemáticas abrangentes. O arredondamento pode envolver regras diferentes – por exemplo, "arredondar metade para cima" (o método usual) ou "arredondar metade para par" (muitas vezes usado em cálculos financeiros para evitar viés).

O Gleam, sendo uma linguagem jovem com raízes no Erlang, depende do robusto conjunto de funções numéricas do Erlang. À medida que a linguagem cresce, podemos ver funções nativas sendo introduzidas, reduzindo a necessidade de chamar rotinas externas.

## Veja Também
- Módulo :math do Erlang para mais cálculos numéricos: https://erlang.org/doc/man/math.html
- Para entender por que o arredondamento pode ser complicado, o Padrão IEEE de Ponto Flutuante: https://ieeexplore.ieee.org/document/8766229
- Interessado na matemática por trás disso? Confira "O Que Todo Cientista da Computação Deve Saber Sobre Aritmética de Ponto Flutuante": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
