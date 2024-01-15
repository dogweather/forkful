---
title:                "Verificando se um diretório existe"
html_title:           "Ruby: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Você já se encontrou em uma situação em que precisava verificar se um diretório existia antes de executar determinada ação? Ao aprender como checar a existência de um diretório em Ruby, você pode evitar erros e tornar seu código mais robusto e confiável.

## Como fazer

Para checar a existência de um diretório em Ruby, podemos utilizar o método "exist?" da classe "Dir". Veja um exemplo abaixo:

```ruby
# Verificando se o diretório "imagens" existe
if Dir.exist?("imagens")
  puts "O diretório 'imagens' existe!"
else
  puts "O diretório 'imagens' não existe."
end
```

Caso o diretório "imagens" exista, a saída do código será "O diretório 'imagens' existe!". Caso contrário, a saída será "O diretório 'imagens' não existe.".

## Aprofundando-se

Além do método "exist?", também podemos utilizar o método "directory?", que retorna "true" caso o caminho fornecido corresponda a um diretório. Veja um exemplo:

```ruby
# Verificando se o caminho fornecido é um diretório
if File.directory?("imagens/paisagem.jpg")
  puts "O caminho fornecido é um diretório."
else
  puts "O caminho fornecido não é um diretório."
end
```

Nesse caso, a saída será "O caminho fornecido não é um diretório.", já que na verdade o caminho aponta para um arquivo.

## Veja também

- Documentação oficial do método "exist?" na classe "Dir": https://ruby-doc.org/core/Dir.html#method-c-exist-3F
- Artigo sobre a classe "Dir": https://www.rubyguides.com/2018/10/directory-class/
- Discussão sobre diferentes formas de checar a existência de um diretório em Ruby: https://stackoverflow.com/questions/5081098/in-ruby-how-do-i-check-if-a-directory-exists-tried-dir-exist-and-file-exist