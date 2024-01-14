---
title:    "Ruby: Lendo argumentos da linha de comando"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que

Você já se deparou com um programa em Ruby que solicitava que você inserisse argumentos na linha de comando? Talvez você já tenha se perguntado qual é a finalidade disso ou por que isso é necessário. Neste artigo, vamos explorar o motivo pelo qual ler argumentos na linha de comando pode ser uma habilidade valiosa para programadores de Ruby.

## Como fazer

Ler argumentos na linha de comando é muito simples em Ruby. Tudo o que precisamos fazer é usar a variável global ``ARGV``, que contém uma lista de todos os argumentos passados na linha de comando. Vamos dar uma olhada em um exemplo básico:

```Ruby
# exemplo.rb
puts "Olá #{ARGV[0]}!"
```

Agora, se executarmos ``ruby exemplo.rb mundo``, o programa irá imprimir ``Olá mundo!`` na tela. Simples, né? Mas o que acontece se não passarmos nenhum argumento na linha de comando? A variável ``ARGV`` será uma lista vazia, então é importante verificar se existem argumentos antes de acessar o índice da lista. Vamos dar uma olhada em outro exemplo:

```Ruby
# exemplo2.rb
if !ARGV.empty?
  puts "Olá #{ARGV[0]}!"
else
  puts "Olá mundo!"
end
```

Agora, se executarmos ``ruby exemplo2.rb Ruby``, o programa imprimirá ``Olá Ruby!``. Mas se executarmos ``ruby exemplo2.rb``, o programa imprimirá ``Olá mundo!``.

Podemos até mesmo fazer algo mais avançado, como fornecer opções para os argumentos na linha de comando. Vamos ver um exemplo:

```Ruby
# exemplo3.rb
nome = nil
idade = nil

ARGV.each_with_index do |arg, index|
  case arg
  when "-n"
    nome = ARGV[index + 1]
  when "-i"
    idade = ARGV[index + 1]
  end
end

if nome && idade
  puts "Olá #{nome}, você tem #{idade} anos!"
elsif nome
  puts "Olá #{nome}!"
else
  puts "Olá mundo!"
end
```

Neste exemplo, podemos passar as opções ``-n`` e ``-i`` após o nome do arquivo e, em seguida, o valor correspondente para cada opção. Por exemplo, se executarmos ``ruby exemplo3.rb -n Maria -i 25``, o programa imprimirá ``Olá Maria, você tem 25 anos!``.

## Deep Dive

Agora que sabemos como ler argumentos na linha de comando, podemos explorar ainda mais essa funcionalidade. Por exemplo, pode ser útil usar a gem [OptParse](https://github.com/ruby/optparse) para analisar e lidar com opções mais complexas na linha de comando. Também é importante lembrar de tratar os argumentos da forma correta, usando conversões de tipo e tratando erros quando necessário.

## Veja também

- [Os Melhores Truques para Argumentos na Linha de Comando em Ruby](https://hackernoon.com/the-great-grand-daddy-of-all-command-line-hacks-cdf3d1436b2d)
- [Como Ler Argumentos para um Programa Ruby a Partir da Linha de Comando](https://www.wikihow.com/Read-Command-Line-Arguments-in-Ruby)