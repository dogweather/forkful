---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:14.014719-07:00
description: "Como fazer: A biblioteca padr\xE3o do Ruby oferece m\xE9todos diretos\
  \ para verificar a exist\xEAncia de um diret\xF3rio. Veja como fazer isso com Ruby\
  \ puro, sem\u2026"
lastmod: '2024-03-13T22:44:47.109469-06:00'
model: gpt-4-0125-preview
summary: "A biblioteca padr\xE3o do Ruby oferece m\xE9todos diretos para verificar\
  \ a exist\xEAncia de um diret\xF3rio."
title: "Verificando se um diret\xF3rio existe"
weight: 20
---

## Como fazer:
A biblioteca padrão do Ruby oferece métodos diretos para verificar a existência de um diretório. Veja como fazer isso com Ruby puro, sem necessidade de bibliotecas de terceiros:

```ruby
require 'fileutils'

# Verificar se um diretório existe
if Dir.exist?('/caminho/para/diretório')
  puts 'O diretório existe.'
else
  puts 'O diretório não existe.'
end
```
Saída de Exemplo:
```
O diretório existe.
```
Ou:
```
O diretório não existe.
```

Além de usar `Dir.exist?`, você também pode utilizar o método `File.directory?`, que retorna `true` se o caminho fornecido for um diretório:

```ruby
if File.directory?('/caminho/para/diretório')
  puts 'O diretório existe.'
else
  puts 'O diretório não existe.'
end
```
Tanto `Dir.exist?` quanto `File.directory?` fazem parte da biblioteca padrão do Ruby e não requerem gems externas para usar, tornando-os opções convenientes e eficientes para verificações de diretórios.
