---
title:                "Lendo argumentos da linha de comando"
date:                  2024-01-20T17:57:11.248285-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo argumentos da linha de comando"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Ler argumentos da linha de comando é simplesmente capturar entradas fornecidas durante a execução de um programa. Programadores fazem isso para permitir que os usuários passem informações customizadas diretamente para os scripts, tornando-os mais flexíveis e interativos.

## Como Fazer:
Para pegar os argumentos, você acessa o array `ARGV` que o Ruby fornece. Veja como isso funciona:

```Ruby
# salve este script como hello.rb
argumentos = ARGV
puts "Número de argumentos: #{argumentos.size}"
argumentos.each_with_index do |argumento, indice|
  puts "Argumento #{indice+1}: #{argumento}"
end
```

Execute o script na linha de comando assim:

```bash
ruby hello.rb Oi Mundo isso é Ruby
```

Saída esperada:

```
Número de argumentos: 4
Argumento 1: Oi
Argumento 2: Mundo
Argumento 3: isso
Argumento 4: é
Argumento 5: Ruby
```

## Mergulho Profundo:
Ler argumentos de linha de comando é uma prática tão antiga quanto os próprios sistemas operacionais. No Ruby, `ARGV` é um array especial para isso, mas existem outras ferramentas como `OptionParser` e gemas como `Thor` para gerenciar argumentos mais complexos. Por baixo dos panos, quando seu script Ruby começa a rodar, o interpretador já populou `ARGV` com os argumentos passados, sem a necessidade de chamada de inicialização especial.

## Veja Também:
Para se aprofundar ainda mais nesse assunto, confira:

- Um guia para o OptionParser, útil para lidar com argumentos de linha de comando mais complexos: [Ruby Docs - OptionParser](https://ruby-doc.org/stdlib-3.0.0/libdoc/optparse/rdoc/OptionParser.html)
- Uma documentação sobre a gema Thor, uma boa alternativa para criar interfaces de linha de comando: [Thor GitHub repository](https://github.com/rails/thor)
