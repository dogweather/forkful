---
title:    "Ruby: Escrevendo para o erro padrão"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que escrever para o "standard error"

Escrever para o "standard error", ou "erro padrão", é uma habilidade importante para qualquer programador. Quando um programa encontra um erro, ele geralmente é enviado para o "standard error", que é uma saída padrão para o acompanhamento de erros e problemas.

## Como fazer

Para escrever para o "standard error" em Ruby, primeiro você precisa acessar o objeto "STDERR". Em seguida, use o método "puts" para imprimir sua mensagem de erro. Aqui está um exemplo de código em Ruby:

```ruby
STDERR.puts "Houve um erro ao executar o programa."
```

O output deste código será algo como:

```
Houve um erro ao executar o programa.
```

## Mergulho profundo

Escrever para o "standard error" é especialmente útil quando se trabalha com bibliotecas e frameworks, pois permite que os erros sejam capturados e registrados de forma mais eficiente. Além disso, é possível manipular esses registros de erro para fornecer informações mais detalhadas ao usuário.

Outra vantagem de escrever para o "standard error" é que ele não é afetado por redirecionamentos de saída. Isso significa que mesmo que a saída padrão seja redirecionada para um arquivo ou outra saída, os erros ainda serão exibidos no console.

## Veja também

- [Documentação oficial do Ruby sobre STDERR](https://ruby-doc.org/core-2.7.1/IO.html#method-c-const_get)
- [Artigo sobre o uso do "standard error" em Ruby](https://www.rubyguides.com/2019/04/ruby-stderr-stdout/)
- [Exemplo prático de escrita para o "standard error" em um projeto Ruby](https://github.com/rails/rails/blob/master/activerecord/lib/active_record/rescue.rb)