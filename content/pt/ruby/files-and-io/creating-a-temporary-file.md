---
date: 2024-01-20 17:41:06.378496-07:00
description: "Criar um arquivo tempor\xE1rio \xE9 criar um arquivo que \xE9 necess\xE1\
  rio apenas enquanto seu programa est\xE1 rodando. Programadores fazem isso para\
  \ n\xE3o correr o\u2026"
lastmod: '2024-03-13T22:44:47.114558-06:00'
model: gpt-4-1106-preview
summary: "Criar um arquivo tempor\xE1rio \xE9 criar um arquivo que \xE9 necess\xE1\
  rio apenas enquanto seu programa est\xE1 rodando. Programadores fazem isso para\
  \ n\xE3o correr o\u2026"
title: "Criando um arquivo tempor\xE1rio"
---

{{< edit_this_page >}}

## O Que & Porquê?

Criar um arquivo temporário é criar um arquivo que é necessário apenas enquanto seu programa está rodando. Programadores fazem isso para não correr o risco de sobrescrever arquivos permanentes e economizar espaço de disco, eliminando-os quando não são mais necessários.

## Como Fazer:

O Ruby possui uma biblioteca padrão chamada `Tempfile` que simplifica a criação de arquivos temporários. Veja como usar:

```Ruby
require 'tempfile'

Tempfile.create('meu_temp') do |tempfile|
  puts "O nome do arquivo temporário é #{tempfile.path}"
  tempfile.write('Olá, mundo!')
  tempfile.rewind
  puts tempfile.read #=> "Olá, mundo!"
end # O arquivo é automaticamente excluído aqui
```

Esse código cria um arquivo temporário, escreve "Olá, mundo!" nele, lê o conteúdo e então fecha e exclui o arquivo automaticamente.

## Mergulho Profundo:

Antes da biblioteca `Tempfile`, os programadores criavam arquivos temporários manualmente, o que podia ser arriscado. A classe `Tempfile` do Ruby abstrai essa complexidade, gerando um nome único e cuidando da remoção do arquivo após o uso.

Alternativas incluem o uso de bancos de dados em memória ou armazenagem em cache, se você estiver procurando desempenho e não quiser lidar com a limpeza de arquivos.

Quanto aos detalhes de implementação, `Tempfile` cria arquivos dentro do diretório temporário do sistema, que você pode encontrar com `Dir.tmpdir`. Ela também habilita a manipulação dos arquivos temporários como qualquer objeto IO em Ruby.

## Veja Também:

- Guia para manipulação de arquivos em Ruby: [Ruby File IO](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)
