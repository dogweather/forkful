---
title:                "Removendo aspas de uma string"
date:                  2024-01-26T03:39:02.620250-07:00
model:                 gpt-4-0125-preview
simple_title:         "Removendo aspas de uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Remover aspas de uma string significa despir aquelas camadas extras - as aspas - dos seus dados de texto. Programadores fazem isso para sanitizar a entrada, preparar strings para processamento, ou apenas para manter as coisas arrumadas e consistentes em suas aplicações. No fim das contas, trata-se de dados limpos e utilizáveis.

## Como fazer:
Remover aspas em Gleam é simples. Podemos usar correspondência de padrões ou funções de string integradas. Aqui está um exemplo rápido para ilustrar:

```gleam
pub fn remove_quotes(text: String) -> String {
  let without_quotes = string.trim(text, "\"")
  without_quotes
}

pub fn main() {
  let text_with_quotes = "\"Olá, Mundo!\""
  let cleaned_text = remove_quotes(text_with_quotes)
  io.println(cleaned_text)
}
```

Saída de exemplo:
```
Olá, Mundo!
```

## Aprofundamento
Historicamente, lidar com aspas em strings tem sido uma tarefa comum no processamento de texto e linguagens de script. Devido à natureza das strings frequentemente serem entrada de usuário ou lidas de arquivos, elas podem vir com aspas que precisam ser removidas por vários motivos, como inserção em banco de dados ou formatação.

Em Gleam, usamos a função `string.trim` para aparar as aspas. Há alternativas! Poderíamos percorrer a string ou aplicar expressões regulares, mas `string.trim` é sua ferramenta prática para o trabalho devido à sua brevidade e desempenho.

Se mergulharmos nos detalhes da implementação, `string.trim` funciona removendo caracteres do início e do final da string que correspondam ao padrão fornecido. Então, se você tem aspas nas duas extremidades da sua string, elas são cortadas de uma só vez. Tenha em mente que ela só remove as aspas se estiverem nas bordas; aspas localizadas confortavelmente no meio do seu texto permanecerão no lugar.

## Veja também
Para as mentes curiosas lá fora que querem explorar mais:
- [Documentação do módulo String do Gleam](https://gleam.run/stdlib/string/)
- Discussões sobre processamento de texto em programação no [Stack Overflow](https://stackoverflow.com/questions/tagged/text-processing)
