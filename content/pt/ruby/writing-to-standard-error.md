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

## O que é isso e por quê?
Escrever no erro padrão é uma técnica usada por programadores para enviar mensagens de erro específicas para a tela do usuário quando um código apresenta um problema ou falha durante sua execução. Isso ajuda a identificar e corrigir erros com mais facilidade, dando informações detalhadas sobre o que causou o problema.

## Como fazer:
Para escrever no erro padrão em Ruby, usamos o método `STDERR.puts` e passamos uma string como argumento. Isso enviará a mensagem para a tela do usuário. Veja abaixo um exemplo:

```ruby
STDERR.puts "Erro: variável não definida"
```
```
Erro: variável não definida
```
Essa técnica também pode ser utilizada em conjunto com o `raise`, que é utilizado para gerar uma exceção em caso de erro. Veja um exemplo:

```ruby
def dividir(x, y)
  raise "O segundo número não pode ser zero" if y == 0
  x / y
end

dividir(10, 0)
```
```
RuntimeError: O segundo número não pode ser zero
```

## Aprofundando:
Escrever no erro padrão é uma prática comum em programação, sendo uma extensão natural da ideia de depuração de código. Antes do surgimento de linguagens de programação modernas, era necessário usar técnicas mais complexas para informar o usuário sobre erros, como a impressão direta no console do sistema.

Uma alternativa ao uso do `STDERR.puts` é utilizar o método `warn`, que também envia uma mensagem de erro para a tela do usuário, mas de uma forma mais amigável. A implementação deste método é baseada no `STDERR.puts`, então ambos acabam tendo o mesmo resultado final.

## Veja também:
Para saber mais sobre a escrita no erro padrão em Ruby, veja a documentação oficial: https://ruby-doc.org/core-3.0.0/IO.html#method-i-puts

Você também pode aprender mais sobre depuração de código em geral no site da Caelum: https://www.caelum.com.br/apostila-ruby-on-rails/usando-o-protractor-desenvolvimento-orientado-a-testes-javascript-com-jasmine-e-protractor/#9-10-depurando-o-codigo

E para ficar por dentro das novidades e dicas sobre programação, acesse o blog do Ruby Brasil: https://www.rubyonrails.com.br/