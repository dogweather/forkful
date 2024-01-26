---
title:                "Escrevendo no erro padrão"
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que É & Porquê?

Escrever para o erro padrão (standard error - stderr) é o ato de enviar mensagens de erro para um stream específico, separando-as da saída normal (stdout). Programadores fazem isso para diagnosticar problemas e manter as saídas de erro e sucesso organizadas e fáceis de analisar.

## Como Fazer:

Em Ruby, você pode usar `$stderr` ou `STDERR` para escrever no erro padrão.

```ruby
puts "Isso vai para a saída padrão"
$stderr.puts "Isso vai para o erro padrão"

# Saída esperada:
# Isso vai para a saída padrão
# Isso vai para o erro padrão (será mostrado em stderr)
```

Você também pode redirecionar `STDERR` para um arquivo:

```ruby
STDERR.reopen("erros.log", "w")
STDERR.puts "Isso será escrito no arquivo erros.log"
# Nenhuma saída esperada na tela, verifique o arquivo erros.log
```

## Aprofundando:

Historicamente, separar stdout e stderr permitiu que usuários e outras aplicações diferenciassem saídas regulares de mensagens de erro. Em Unix, isso se tornou uma prática padrão. Alternativas ao uso direto de `STDERR` em Ruby incluem o uso de bibliotecas de logging, como a `Logger`, que oferecem mais flexibilidade. Internamente, `STDERR` é uma constante global que é uma instância da classe `IO`, pré-configurada para escrever para o erro padrão do sistema operacional.

## Veja Também:

- Documentação oficial do Ruby sobre I/O: [https://ruby-doc.org/core-3.0.0/IO.html](https://ruby-doc.org/core-3.0.0/IO.html)
- Guia sobre a classe Logger: [https://ruby-doc.org/stdlib-3.0.0/libdoc/logger/rdoc/Logger.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/logger/rdoc/Logger.html)
