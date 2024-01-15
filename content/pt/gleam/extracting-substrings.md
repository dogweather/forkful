---
title:                "Extraindo subtrings"
html_title:           "Gleam: Extraindo subtrings"
simple_title:         "Extraindo subtrings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que
 São muitas as situações em que precisamos extrair substrings de uma string maior. Seja para validar dados de entrada, manipular partes específicas de um texto ou otimizar o processamento de informações, é comum a necessidade de trabalhar com partes menores de uma string. Através do processo de extração de substrings, é possível obter exatamente as informações que precisamos, reduzindo o tempo e esforço necessário para realizar determinadas tarefas.

## Como Fazer
Extrair substrings em Gleam é bastante simples e flexível. Para começar, você precisará da string original de onde deseja extrair a substring e de um índice inicial e final que delimitem qual parte você deseja obter. Veja o exemplo abaixo para entender melhor:

````Gleam
fn main() {
  // string original
  let string = "Olá, mundo!";
  
  // índices de início e fim
  let start = 5;
  let end = 11;
  
  // extração da substring
  let substring = string[start..end];
  
  // exibindo o resultado
  io.println(substring); // irá imprimir "mundo"
}
````

É importante notar que o índice de início é inclusivo, ou seja, a letra no índice indicado será incluída na substring final. Já o índice final é exclusivo, ou seja, a letra no índice indicado não será incluída na substring final. Isso permite que você trabalhe com intervalos precisos e evite erros de indexação.

Além disso, é possível utilizar índices negativos, que contam a partir do final da string. Por exemplo, se você utilizar o índice -1 no lugar do índice final, o último caractere será incluído na substring final. Isso é útil quando a string tem um tamanho desconhecido ou variável.

## Aprofundando-se
Além dos aspectos básicos da extração de substrings apresentados anteriormente, é importante conhecer algumas outras funcionalidades importantes. Primeiramente, é possível utilizar variáveis no lugar dos índices de início e fim, o que possibilita definir esses valores dinamicamente durante a execução do programa.

Também é possível utilizar o método `.len()` em uma string para obter o seu comprimento, o que pode ajudar a definir índices finais dinamicamente ou verificar se um índice é válido. Além disso, é possível utilizar os conceitos de Pattern Matching para extrair substrings em Gleam de maneira ainda mais precisa e avançada.

## Veja Também
- Documentação oficial sobre extração de substrings em Gleam: https://gleam.run/documentation/stdlib#substr
- Exemplos de aplicação de extração de substrings em projetos reais: https://github.com/search?q=language%3Agleam+substring&type=Repositories