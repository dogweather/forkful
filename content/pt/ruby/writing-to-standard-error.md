---
title:                "Ruby: Escrevendo para o erro padrão"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para erro padrão?

Se você é um programador Ruby, com certeza já ouviu falar sobre a saída padrão e a saída de erro padrão. Enquanto a saída padrão é usada para exibir resultados e mensagens relevantes ao usuário, a saída de erro padrão é usada para exibir mensagens de erro e falhas no código. Mas por que devemos nos preocupar em escrever para o erro padrão? A resposta é simples: para garantir que nosso código esteja funcionando corretamente e para facilitar a solução de problemas em caso de erros.

## Como fazer?

Escrever para erro padrão no Ruby é bem simples. Basta utilizar o método `STDERR.puts()` seguido da mensagem que você deseja exibir. Veja um exemplo abaixo:

```Ruby
STDERR.puts("Ops, algo deu errado!")
```

Este código irá exibir a mensagem "Ops, algo deu errado!" na saída de erro padrão. Você também pode utilizar a sintaxe de `warn()` para escrever para o erro padrão. Veja:

```Ruby
warn("Cuidado, esse código está com um erro!")
```

Além disso, você também pode utilizar variáveis dentro da mensagem. Veja:

```Ruby
num = 5
STDERR.puts("O número #{num} é inválido.")
```

A saída seria "O número 5 é inválido." na saída de erro padrão. Agora que você sabe como escrever para o erro padrão, vamos nos aprofundar um pouco mais.

## Mergulho profundo no erro padrão

Além de exibir mensagens de erro, o uso do erro padrão também é útil para fazer logs de erros em seus próprios programas. Você pode criar um arquivo de log e escrever a saída de erro padrão nele ao invés de exibi-la apenas na tela. Isso pode ser útil para verificar erros em momentos específicos no código e também para manutenção futura.

Outra opção interessante é redirecionar a saída de erro padrão para um arquivo, utilizando o símbolo `>`. Por exemplo:

```Ruby
ruby meu_programa.rb > erro.log
```

Isso irá criar um arquivo "erro.log" contendo todas as mensagens de erro que foram exibidas durante a execução do programa. Isso pode ajudar na depuração de problemas e também facilita a identificação de erros recorrentes.

## Veja também

- [Documentação oficial do Ruby para saída de erro padrão](https://ruby-doc.org/core-2.6/IO.html#method-c-new-label-Standard+Streams)
- [Artigo sobre saída padrão e saída de erro padrão no Ruby](https://www.rubyguides.com/2016/08/ruby-print-vs-puts/)
- [Tutorial sobre como fazer logs de erros no Ruby](https://www.rubyguides.com/2016/04/debugging-logs-basic-guide-ruby/)