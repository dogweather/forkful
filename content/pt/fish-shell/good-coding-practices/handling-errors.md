---
date: 2024-01-26 00:52:24.550950-07:00
description: "O tratamento de erros permite que seu script lide com o inesperado de\
  \ forma elegante. Fazemos isso para gerenciar falhas sem deixar os cabelos dos nossos\u2026"
lastmod: '2024-03-13T22:44:47.013439-06:00'
model: gpt-4-1106-preview
summary: "O tratamento de erros permite que seu script lide com o inesperado de forma\
  \ elegante. Fazemos isso para gerenciar falhas sem deixar os cabelos dos nossos\u2026"
title: Tratamento de erros
---

{{< edit_this_page >}}

## O Quê & Por Quê?
O tratamento de erros permite que seu script lide com o inesperado de forma elegante. Fazemos isso para gerenciar falhas sem deixar os cabelos dos nossos usuários ficarem grisalhos.

## Como fazer:
Para capturar erros no Fish, conte com o comando `status` e condicionais. Digamos que o `ping` falhe; veja como detectar isso:

```fish
ping -c 1 example.com
if not status is-success
    echo "Algo estranho aconteceu com o ping."
end
```

Saída de exemplo se o `ping` falhar:

```
Algo estranho aconteceu com o ping.
```

Para lidar com um código de erro específico, use `status --is`:

```fish
false
if status --is 1
    echo "Capturado um erro com código 1."
end
```

Saída de exemplo:
```
Capturado um erro com código 1.
```

Para uma abordagem mais robusta, considere usar uma função:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping falhou com status $status"
        return 1
    end
end

try_ping
```

## Mergulho Profundo
O tratamento de erros no Fish não corresponde ao paradigma `try/catch` que você pode conhecer de linguagens de alto nível. Em vez disso, temos statuses de saída diretos fornecidos pelo comando `status`.

Historicamente, em sistemas do tipo Unix, um status de saída `0` significa sucesso, enquanto qualquer valor não nulo indica um erro, o que geralmente reflete diferentes razões de falha. Esta convenção é empregada pela maioria das utilidades de linha de comando e, portanto, pelo próprio Fish.

Alternativas para verificações de `status` no Fish incluem o tratamento de sinais via `trap` em outros shells, mas o Fish prefere uma verificação de status mais explícita, porque é mais limpa e menos propensa a efeitos colaterais.

Em termos de implementação, o tratamento de erros no Fish permanece simples e poderoso, em grande parte devido à sua natureza não bloqueadora e ênfase em sintaxe clara, como mostrado nos exemplos. Os códigos de erro se integram bem com as funções, permitindo uma gestão de erros modular e legível.

## Veja também
- Documentação do Fish sobre condicionais: https://fishshell.com/docs/current/language.html#conditionals
- Tutorial do Fish sobre tratamento de erros: https://fishshell.com/docs/current/tutorial.html#error-handling
