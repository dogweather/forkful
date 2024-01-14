---
title:    "Ruby: Lendo argumentos da linha de comando"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando é importante

Se você é um programador Ruby, provavelmente já ouviu falar sobre a leitura de argumentos da linha de comando. Mas por que isso é importante? A resposta é simples: ao usar argumentos da linha de comando, você pode fornecer entrada para o seu programa de forma dinâmica, sem precisar alterar o código fonte toda vez que quiser alterar os valores.

## Como ler argumentos da linha de comando em Ruby

A leitura de argumentos da linha de comando em Ruby é bem simples e pode ser feita usando o objeto `ARGV`. Veja um exemplo abaixo:

```ruby
# salvando os argumentos em uma variável
args = ARGV

# acessando o primeiro argumento
primeiro_argumento = args[0]

# acessando o segundo argumento
segundo_argumento = args[1]

puts "O primeiro argumento é #{primeiro_argumento} e o segundo é #{segundo_argumento}."
```

Se executarmos esse código com `ruby argumentos.rb primeiro segundo`, a saída seria:

```
O primeiro argumento é primeiro e o segundo é segundo.
```

## Aprofundando na leitura de argumentos da linha de comando

Além do objeto `ARGV`, podemos usar a gem `optparse` para facilitar a leitura e o gerenciamento de argumentos da linha de comando. Com ela, podemos especificar opções e argumentos esperados, obter ajuda para o usuário e muito mais. Abaixo, segue um exemplo de uso:

```ruby
require 'optparse'

# definindo as opções esperadas
options = {}
OptionParser.new do |opts|
  opts.banner = "Uso: exemplo.rb [opções]"

  opts.on("-f", "--file FILE", "Nome do arquivo a ser processado") do |f|
    options[:file] = f
  end

  opts.on("-n", "--number NUMBER", "Número a ser multiplicado") do |n|
    options[:number] = n
  end
end.parse!

# acessando as opções e exibindo informações para o usuário
puts "Arquivo a ser processado: #{options[:file]}"
puts "Número a ser multiplicado: #{options[:number]}"
```

Executando esse código com `ruby argumentos.rb -f arquivo.txt -n 5`, teríamos como saída:

```
Arquivo a ser processado: arquivo.txt
Número a ser multiplicado: 5
```

## Veja também

- [Documentação oficial sobre a leitura de argumentos da linha de comando em Ruby](https://ruby-doc.org/core-2.7.1/classes/ARGV.html)
- [Documentação da gem Optparse](https://github.com/ruby/optparse)
- [Artigo sobre a leitura de argumentos em linha de comando com Ruby](https://www.simonecarletti.com/blog/2009/09/inside-the-ruby-runtime-reading-arguments-from-the-command-line/)