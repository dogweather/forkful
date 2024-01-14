---
title:    "Ruby: Escrevendo no erro padrão"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Escrever para o erro padrão é uma habilidade importante para qualquer programador de Ruby. Isso permite que você saiba quando algo deu errado em seu programa e fornece informações úteis para depuração. Também ajuda a melhorar a qualidade do código, tornando-o mais robusto e confiável.

## Como Fazer

Existem algumas maneiras de escrever para o erro padrão em Ruby. Uma delas envolve o uso da função `puts` juntamente com o objeto `STDERR`. Veja um exemplo abaixo:

```Ruby
puts "Algo deu errado" if x == nil
STDERR.puts "Variável x é nula"
```

A saída desse código seria:

```
Variável x é nula
```

Isso significa que a mensagem "Algo deu errado" será impressa no fluxo de saída padrão, enquanto "Variável x é nula" será impressa no fluxo de erro padrão.

Outra maneira é usar a classe `Logger` do Ruby, que oferece mais opções de configuração. Veja um exemplo usando o `Logger` abaixo:

```Ruby
require 'logger'
logger = Logger.new(STDERR)
logger.error("Algo deu errado")
```

A saída desse código seria:

```
E, [2018-02-01T13:45:50.781859 #9578] ERROR -- : Algo deu errado
```

Aqui, a mensagem de erro foi formatada automaticamente e inclui o data e hora em que foi registrado.

## Viagem Profunda

Ao escrever para o erro padrão, é importante lembrar que você também pode personalizar a saída, adicionando informações extras para ajudar na depuração. Além disso, você também pode redirecionar o fluxo de erro para um arquivo, se necessário.

Por exemplo, se você estiver desenvolvendo um script que realiza uma operação em um banco de dados, pode ser útil registrar erros nesse script em um arquivo de log, em vez de apenas vê-los no console. Isso é útil, pois torna mais fácil rastrear os problemas e corrigi-los posteriormente.

## Veja Também

- [Documentação Oficial do Ruby sobre a Classe Logger](https://ruby-doc.org/stdlib-2.5.0/libdoc/logger/rdoc/Logger.html)
- [Artigo sobre Logging em Ruby](https://medium.com/rubyinside/logging-in-ruby-cf8b94c5e946)
- [Vídeo: How to write to standard error in Ruby](https://www.youtube.com/watch?v=CfCgUMJwBoo)