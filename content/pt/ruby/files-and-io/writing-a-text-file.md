---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:54.798102-07:00
description: "Como fazer: Ruby torna as opera\xE7\xF5es com arquivos simples. Para\
  \ escrever em um arquivo, voc\xEA pode usar a classe `File` incorporada ao Ruby.\
  \ O exemplo a\u2026"
lastmod: '2024-03-13T22:44:47.113386-06:00'
model: gpt-4-0125-preview
summary: "Ruby torna as opera\xE7\xF5es com arquivos simples."
title: Escrevendo um arquivo de texto
weight: 24
---

## Como fazer:
Ruby torna as operações com arquivos simples. Para escrever em um arquivo, você pode usar a classe `File` incorporada ao Ruby. O exemplo a seguir demonstra como abrir um arquivo para escrita (modo `"w"`) e para adição (modo `"a"`), em seguida, escrever uma string nele e garantir que o arquivo seja fechado posteriormente:

```ruby
# Escrevendo novo conteúdo em um arquivo, sobrescrevendo o conteúdo existente
File.open("example.txt", "w") do |file|
  file.puts "Olá, Ruby!"
end

# Adicionando conteúdo ao final de um arquivo
File.open("example.txt", "a") do |file|
  file.puts "Adicionando outra linha."
end
```
Após executar ambos os trechos, o conteúdo de `example.txt` será:
```
Olá, Ruby!
Adicionando outra linha.
```

### Usando uma biblioteca de terceiros: FileUtils
Para operações com arquivos mais complexas, a biblioteca padrão do Ruby `FileUtils` pode ser útil, embora para a escrita básica de arquivos, os métodos padrão da `File` sejam suficientes. No entanto, se você deseja copiar, mover, remover ou realizar outras operações no sistema de arquivos em conjunto com a escrita de arquivos, vale a pena explorar o `FileUtils`.

Um exemplo do uso de `FileUtils` para criar um diretório e, em seguida, escrever em um arquivo dentro desse diretório:
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/today.log", "w") do |file|
  file.puts "Entrada do log: #{Time.now}"
end
```

Isso demonstra a criação de um novo diretório `logs`, caso ele ainda não exista, e a escrita em um novo arquivo `today.log` dentro dele, mostrando a manipulação de diretórios e arquivos sem escrever diretamente com o FileUtils, mas utilizando sua capacidade de manipulação de diretórios.
