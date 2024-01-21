---
title:                "Concatenando strings"
date:                  2024-01-20T17:34:36.461792-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenando strings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Concatenar strings é basicamente juntar duas ou mais sequências de caracteres para formar uma única string. Programadores fazem isso para construir mensagens, combinar dados ou para formatar outputs de uma maneira desejada.

## Como Fazer:
```gleam
fn main() {
  let saudacao = "Olá, "
  let nome = "Mundo!"
  let mensagem = saudacao ++ nome
  io.println(mensagem)
}
```
Output:
```
Olá, Mundo!
```

## Aprofundando:
Historicamente, a possibilidade de concatenar strings tem sido um recurso básico na maioria das linguagens de programação, pois é essencial na manipulação e no display de texto. No Gleam, como em muitas linguagens funcionais modernas, concatenar é feito usando operadores simples como `++`, que torna o código limpo e legível. Alternativas à concatenação incluem a interpolação de strings ou a construção de strings com funções específicas ou módulos, que podem ser mais eficientes dependendo do contexto. Detalhes de implementação no Gleam são otimizados para garantir que a concatenação seja tão eficiente quanto possível, mas como em qualquer operação, abusar da concatenação em loops extensos pode levar a problemas de desempenho.

## Veja Também:
- Tutorial sobre interpolação e formatação de strings em Gleam: [gleam.run](https://gleam.run/book/tour/functions.html#string-interpolation)
- GitHub do projeto Gleam para quem quiser contribuir ou entender mais sobre o funcionamento interno: [github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)