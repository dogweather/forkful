---
title:                "Escrevendo um arquivo de texto"
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Escrever um arquivo de texto em Ruby significa gravar dados em um arquivo no disco. Programadores fazem isso para persistir informações entre sessões do programa, log de eventos, ou para criar configurações e documentos.

## Como Fazer:

Para escrever em um arquivo de texto em Ruby, você pode usar a classe `File` e o método `write` ou `puts`. Veja o exemplo:

```ruby
File.open('exemplo.txt', 'w') do |arquivo|
  arquivo.puts 'Olá, Mundo!'
end
```

Isso cria (ou sobrescreve se já existir) um arquivo chamado `exemplo.txt` com o conteúdo `Olá, Mundo!`. A flag `'w'` indica que você quer escrever no arquivo. Se você quer apenas adicionar conteúdo ao final do arquivo existente, use `'a'`.

## Mergulho Profundo:

Historicamente, a escrita de arquivos é uma das operações de I/O fundamentais em programação. Ruby fornece métodos alternativos, como `IO.write` ou ainda operações de mais baixo nível com `syswrite`. A escolha do método depende do controle que você quer ter e da simplicidade que precisa.

Em termos de desempenho, escrever em um arquivo pode ser uma operação custosa, e Ruby oferece maneiras de otimizar isso, como bufferização implícita ou explícita. É também possível manipular os atributos do arquivo, como permissões, utilizando a classe `File`.

## Veja Também:

- Ruby Doc sobre a classe File: [ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- Um tutorial sobre IO em Ruby: [rubyguides.com/2015/05/working-with-files-ruby/](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- Guia de melhores práticas para IO: [github.com/rubocop/ruby-style-guide#io](https://github.com/rubocop/ruby-style-guide#io)
