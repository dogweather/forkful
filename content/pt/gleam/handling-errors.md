---
title:                "Tratamento de erros"
date:                  2024-01-26T00:53:16.300484-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tratamento de erros"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Tratar erros envolve antecipar possíveis falhas no seu código e gerenciar essas situações de forma graciosa. Programadores fazem isso porque mantém as aplicações robustas e amigáveis ao usuário, mesmo quando ocorrem situações inesperadas.

## Como fazer:
No Gleam, frequentemente você usará o tipo `Result` para tratamento de erros. É um enum com duas variantes: `Ok` (para sucesso) e `Error` (para falha). Aqui está um exemplo simples:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("Oops! It broke.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(message) => {
      io.println(message)
      0
    } 
  }
}
```

Se você executar `main` com `might_fail(False)`, retornará `42`. Se você passar `True`, ele imprime "Oops! It broke." e retorna `0`.

## Aprofundamento
A abordagem do Gleam para tratamento de erros é influenciada por suas raízes em Erlang. Historicamente, Erlang usa uma filosofia de "deixe falhar", contando com árvores de supervisão para gerenciar falhas de processos. No entanto, quando você está escrevendo código Gleam que não está dentro de um processo destinado a ser supervisionado, como dentro de uma função de biblioteca, você deve tratar os erros explicitamente.

Alternativas para o uso do `Result` incluem o tipo `Option` para casos em que algo pode ser `None` (nada) ou `Some` (algo), mas estes não carregam informações sobre o erro. Para sinalizar erros através de limites de processos, você pode usar os mecanismos de passagem de mensagens do Erlang.

O tratamento de erros no Gleam reflete um estilo de programação funcional, onde efeitos colaterais (como erros) são geridos com tipos e correspondência de padrões (pattern-matching), proporcionando clareza e previsibilidade na gestão de erros.

## Veja Também
- [Tratamento de Erros no Erlang](http://erlang.org/doc/reference_manual/errors.html)
