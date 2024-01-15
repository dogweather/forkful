---
title:                "Lendo um arquivo de texto."
html_title:           "Ruby: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Você pode se perguntar por que dedicar seu tempo para aprender a ler um arquivo de texto em Ruby. A resposta é simples - isso é uma habilidade essencial para qualquer programador aprender, pois muitas vezes precisamos acessar e processar dados em formato de texto.

## Como Fazer

Para ler um arquivo de texto em Ruby, primeiro precisamos abrir o arquivo usando o método `File.open`. Em seguida, usamos o método `read` para ler todo o conteúdo do arquivo e armazená-lo em uma variável. Aqui está um exemplo de código:

```Ruby
file = File.open("arquivo.txt")
conteudo = file.read
puts conteudo
```

Se você quiser ler o conteúdo do arquivo linha por linha, podemos usar o método `each_line` em vez do `read`. Aqui está um exemplo:

```Ruby
file = File.open("arquivo.txt")
file.each_line do |line|
  puts line
end
```

Esses são apenas alguns exemplos básicos, mas existem muitas outras maneiras de ler e processar dados de um arquivo de texto em Ruby.

## Aprofundando

Ao ler um arquivo de texto, é importante ter em mente que o conteúdo será lido como uma string. Isso significa que precisamos estar cientes de caracteres especiais, como espaços em branco e quebras de linha.

Para acessar dados específicos em um arquivo de texto, podemos usar métodos como `split` para dividir a string em diferentes partes. Também podemos usar expressões regulares para encontrar padrões específicos no conteúdo do arquivo.

Além disso, é importante saber que existem diferentes opções para abrir um arquivo, como `File.new`, `File.open` e `IO.read`. Cada um desses métodos tem suas próprias nuances e é importante ler a documentação oficial para entender qual método é melhor para sua situação específica.

## Veja Também

- [Documentação oficial do Ruby sobre Arquivos](https://ruby-doc.org/core-3.0.1/File.html)
- [Ruby IO Class](https://www.rubyguides.com/2015/05/io-class-ruby/)
- [Expressões Regulares em Ruby](https://www.rubyguides.com/regex-ruby/)

Esperamos que este artigo tenha ajudado você a entender como ler um arquivo de texto em Ruby. Continue praticando e explorando diferentes métodos para se tornar um especialista em processamento de arquivos em Ruby!