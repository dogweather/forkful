---
aliases:
- /pt/ruby/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:54.798102-07:00
description: "Escrever em um arquivo de texto em Ruby \xE9 uma opera\xE7\xE3o fundamental\
  \ que permite armazenar sa\xEDdas e dados de forma persistente, possibilitando o\
  \ acesso ou a\u2026"
lastmod: 2024-02-18 23:08:58.684085
model: gpt-4-0125-preview
summary: "Escrever em um arquivo de texto em Ruby \xE9 uma opera\xE7\xE3o fundamental\
  \ que permite armazenar sa\xEDdas e dados de forma persistente, possibilitando o\
  \ acesso ou a\u2026"
title: Escrevendo um arquivo de texto
---

{{< edit_this_page >}}

## O Que & Por Quê?
Escrever em um arquivo de texto em Ruby é uma operação fundamental que permite armazenar saídas e dados de forma persistente, possibilitando o acesso ou a modificação desses dados posteriormente. Programadores costumam realizar essa tarefa por motivos como registrar logs, salvar configurações ou exportar dados em um formato legível por humanos.

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
