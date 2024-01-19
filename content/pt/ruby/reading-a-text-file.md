---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lendo um Arquivo de Texto em Ruby

## O quê & para quê?
Ler um arquivo de texto é buscar e interpretar as informações contidas nele. Programadores fazem isso para manipular, extrair e usar essas informações em suas aplicações.

## Como Fazer:
Em Ruby, você pode ler um arquivo de texto de várias maneiras. Aqui está o básico:

```Ruby
# abrindo e lendo um arquivo
file = File.open("meu_arquivo.txt", "r")
dados = file.read
puts dados
file.close
```
Quando você executa este código, verá todo o conteúdo do `meu_arquivo.txt` impresso na tela.

Existem métodos mais sofisticados disponíveis. Por exemplo, o seguinte código lê o arquivo linha por linha:

```Ruby
File.open("meu_arquivo.txt", "r").each_line do |line|
  puts line
end
```
O Ruby executa o bloco para cada linha do arquivo, imprimindo a linha.

## Mergulho Profundo
Ler arquivos é uma prática fundamental na programação desde os primeiros dias do COBOL e FORTRAN. Hoje, a maioria dos aplicativos ainda dependem muito da capacidade de ler e escrever arquivos.

Sobre alternativas, o Ruby oferece algumas outras formas de ler um arquivo. Por exemplo, você pode ler todo o conteúdo de uma vez com `IO.read("meu_arquivo.txt")` ou ler todas as linhas em uma matriz com `IO.readlines("meu_arquivo.txt")`.

No que diz respeito à implementação, o Ruby usa chamadas de sistema nativas para ler arquivos. Isso significa que a maneira como os arquivos são lidos pode variar entre diferentes sistemas operacionais.

## Veja Também
Se você quiser saber mais sobre a leitura de arquivos em Ruby, aqui estão alguns links úteis:
1. [Documentação oficial do IO em Ruby](https://ruby-doc.org/core-3.0.0/IO.html)
2. [Tutorial interativo de manipulação de arquivos em Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-file-i-o-and-serialization)
3. [Tutorial detalhado sobre IO em Ruby](http://zetcode.com/lang/rubytutorial/io/)