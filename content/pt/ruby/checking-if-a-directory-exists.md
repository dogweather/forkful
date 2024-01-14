---
title:    "Ruby: Verificando se um diretório existe."
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Verificar a existência de um diretório é uma tarefa importante na programação, pois garante que o código funcione corretamente e evita erros durante a execução do programa. Também pode ser útil para determinar se um diretório específico precisa ser criado ou não.

## Como fazer a verificação em Ruby

Em Ruby, podemos verificar se um diretório existe usando o método `Dir.exist?`, que retorna um booleano `true` se o diretório existir ou `false` se não existir. Veja um exemplo abaixo:

```ruby
if Dir.exist?("meu_diretorio")
  puts "O diretório existe!"
else
  puts "O diretório não existe."
end
```

O output deste código será `O diretório existe!` se o diretório chamado "meu_diretorio" existir no sistema. Caso contrário, será exibida a mensagem `O diretório não existe.`.

Outra opção é usar o método `Dir.empty?`, que retorna um booleano `true` se o diretório estiver vazio ou `false` se tiver algum arquivo dentro dele. Veja o código a seguir:

```ruby
if Dir.empty?("meu_diretorio")
  puts "O diretório está vazio."
else
  puts "O diretório possui arquivos."
end
```

O output será `O diretório está vazio.` se não houver arquivos no diretório "meu_diretorio". Caso contrário, será `O diretório possui arquivos.`.

## Profundidade sobre a verificação de diretório

Há casos em que é necessário verificar se um diretório existe e criar um novo diretório caso não exista. Podemos fazer isso usando o método `Dir.mkdir`, que cria um novo diretório com o nome especificado. No entanto, é importante lembrar que este método pode retornar um erro se o diretório já existir, então é recomendado incluir uma verificação prévia usando o método `Dir.exist?`.

Além disso, existem outros métodos úteis para manipulação de diretórios em Ruby, como `Dir.chdir`, que permite mudar o diretório atual; `Dir.entries`, que retorna uma lista de arquivos e pastas dentro de um diretório; e `Dir.delete`, que exclui um diretório e todo o seu conteúdo.

## Veja também

- [Documentação oficial do Ruby sobre o módulo `Dir`](https://ruby-doc.org/core-3.0.0/Dir.html)
- [Tutorial sobre verificação de existência de diretórios em Ruby](https://www.rubyguides.com/2015/07/ruby-directory/)
- [Vídeo tutorial sobre manipulação de diretórios em Ruby](https://www.youtube.com/watch?v=WhlYcNSZC7w)