---
title:                "Ruby: Imprimindo saída de depuração"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que
Cada programador, em algum momento da sua carreira, enfrenta problemas ao tentar identificar um erro em seu código. E é aí que entra a impressão de saída de depuração. Ao adicionar mensagens de depuração em seu código, você pode acompanhar o fluxo de execução e identificar onde as coisas estão dando errado. Isso pode economizar muito tempo e energia na resolução de problemas em seu código.

## Como Fazer
Para imprimir saída de depuração em Ruby, você pode usar o método `puts` seguido da mensagem que você deseja imprimir dentro de aspas. Por exemplo:
```Ruby
puts "Entrando no loop for..."
```
Isso irá imprimir a mensagem "Entrando no loop for..." na sua linha de comando ao executar o seu código.

Você também pode adicionar variáveis à sua mensagem de depuração para acompanhar seus valores durante a execução do código. Por exemplo:
```Ruby
contador = 0
puts "O contador atual é: #{contador}"
```
Isso imprimirá a mensagem "O contador atual é: 0", onde 0 é o valor atual da variável contador. Isso pode ser útil para rastrear quaisquer mudanças nos valores das suas variáveis ao longo do tempo.

## Profundidade
Além do método `puts`, também existe o método `p` que pode ser usado para imprimir a saída de depuração em Ruby. A diferença é que o método `p` irá mostrar o tipo de dados de uma variável, enquanto o `puts` apenas imprimirá o valor.

Outra opção é usar o módulo `logger` do Ruby para imprimir saída de depuração. O módulo `logger` oferece opções avançadas, como gravar a saída em um arquivo de log e adicionar níveis de gravidade às mensagens.

Além disso, é importante lembrar de remover todas as mensagens de depuração após resolver os problemas em seu código. Você não quer que sua saída de depuração acabe sendo impressa em produção!

## Veja Também
- [Documentação Oficial do Ruby sobre o método `puts`](https://ruby-doc.org/core-2.7.0/IO.html#method-i-puts)
- [Guia sobre depuração em Ruby](https://www.tutorialspoint.com/ruby/ruby_debugger.htm)
- [Artigo sobre o módulo `logger` do Ruby](https://www.rubyguides.com/2018/08/ruby-logger/)