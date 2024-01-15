---
title:                "Escrevendo para o erro padrão"
html_title:           "Ruby: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Por que
Escrever para a saída padrão de erro (standard error) pode ser útil para solucionar problemas e rastrear erros em um código Ruby. Além disso, é uma boa prática de programação para garantir que seu código esteja funcionando corretamente.

## Como Fazer
Você pode usar o método `puts` para imprimir mensagens na saída padrão de erro. Isso irá encerrar a execução do programa e exibir a mensagem no console. Veja um exemplo abaixo:

```Ruby
puts "Este é um erro de teste"
puts "O programa será encerrado agora"
```

**Saída:**

```
Este é um erro de teste
O programa será encerrado agora
```

Você também pode usar o código de erro `exit` para sair do programa e exibir uma mensagem de erro. Veja o exemplo abaixo:

```Ruby
exit 1, "Houve um erro na execução do programa"
```

**Saída:**

```
Houve um erro na execução do programa
```

## Mergulho Profundo
Existem alguns métodos adicionais que podem ser úteis ao escrever para a saída de erro. Por exemplo, você pode usar `warn` para exibir uma mensagem de aviso com a cor amarela no console. E para imprimir mensagens em vermelho, você pode usar `STDERR.puts` ou `STDERR.print`.

Além disso, você pode personalizar suas mensagens de erro para incluir informações específicas sobre o erro que está ocorrendo. Isso é especialmente útil para depurar e rastrear problemas em seu código.

## Veja Também
- [Documentação oficial do Ruby sobre output de erro](https://ruby-doc.org/core-2.7.1/IO.html#method-i-warn)
- [Exemplos de código do Ruby](https://www.rubyguides.com/ruby-tutorial/puts-vs-print/)