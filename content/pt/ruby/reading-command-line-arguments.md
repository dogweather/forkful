---
title:                "Ruby: Lendo argumentos da linha de comando"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade importante para qualquer programador em Ruby. Com essa habilidade, é possível criar programas interativos e personalizáveis, permitindo que os usuários insiram informações diretamente no momento da execução do programa.

## Como Ler Argumentos da Linha de Comando

Em Ruby, ler argumentos da linha de comando é uma tarefa simples. Primeiro, precisamos definir uma variável para armazenar os argumentos. Isso pode ser feito usando o método `ARGV`, que retorna uma matriz com os argumentos passados para o programa.

```
args = ARGV
puts "Seu primeiro argumento é #{args[0]}"
```

Assim, se o usuário digitar `ruby programa.rb Olá`, o programa imprimirá `Seu primeiro argumento é Olá`.

É importante notar que os argumentos da linha de comando são sempre lidos como strings. Portanto, se você precisar de um número inteiro ou float, será necessário convertê-lo usando o método `to_i` ou `to_f`, respectivamente.

Você também pode definir argumentos padrão caso o usuário não forneça nenhum. Basta adicionar um argumento após uma vírgula no método `ARGV`. Por exemplo:

```
args = ARGV[0] || "Mundo"
puts "Olá #{args}!"
```

Se o usuário não fornecer nenhum argumento, o padrão "Mundo" será usado.

## Profundamente em Leitura de Argumentos da Linha de Comando

Além do método `ARGV`, há também o método `ARGV.getopts`, que permite que você especifique opções para seus argumentos. Por exemplo, imagine que seu programa precise de um argumento `-n` para executar alguma ação específica. Você pode usá-lo da seguinte maneira:

```
options = {}
opt_parser = OptionParser.new do |opts|
  opts.banner = "Uso: programa.rb [opções]"
  opts.on("-n NOME", "--nome=NOME", "Define o nome") do |nome|
    options[:nome] = nome
  end
end
opt_parser.parse!

puts "Olá #{options[:nome]}!"
```

Agora, se o usuário executar `ruby programa.rb -n Maria`, o programa imprimirá `Olá Maria!`. Você pode especificar mais opções conforme necessário.

## See Also

- [Documentação Oficial do Ruby - ARGV](https://ruby-doc.org/core-2.7.3/ARGV.html)
- [Documentação Oficial do Ruby - OptionParser](https://ruby-doc.org/stdlib-2.0.0/libdoc/optparse/rdoc/OptionParser.html)
- [Tutorial do Ruby para Iniciantes - Leitura de Argumentos da Linha de Comando](https://ruby-doc.org/stdlib-2.7.3/libdoc/optparse/rdoc/index.html)