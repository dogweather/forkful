---
title:    "Ruby: Verificando se um diretório existe"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Se você é um programador iniciante, pode se perguntar por que é importante verificar se um diretório existe antes de realizar qualquer operação nele. Bem, a resposta é simples: verificar a existência do diretório pode evitar erros e falhas no seu código.

Imagine que você está desenvolvendo um programa que precisa ler um arquivo em um determinado diretório. Se o diretório não existe, seu código provavelmente irá falhar ou gerar um erro, o que pode ser frustrante e demorado para corrigir. Por isso, é sempre uma boa prática verificar se o diretório existe antes de realizar qualquer operação nele.

## Como verificar se um diretório existe em Ruby

Para verificar se um diretório existe em Ruby, podemos usar o método `Dir.exist?`, que retorna `true` se o diretório existir e `false` se não existir. Podemos utilizar uma estrutura condicional `if` para lidar com esses dois casos.

```Ruby
if Dir.exist?("caminho/do/diretório")
  puts "O diretório existe!"
  #Código para realizar operações no diretório
else
  puts "O diretório não existe."
  #Código para criar o diretório ou lidar com o erro
end
```

Podemos também utilizar o método `File.directory?`, que funciona de maneira semelhante. A diferença é que esse método não aceita o caminho completo do diretório, apenas o nome dele.

```Ruby
if File.directory?("diretório")
  puts "O diretório existe!"
  #Código para realizar operações no diretório
else
  puts "O diretório não existe."
  #Código para criar o diretório ou lidar com o erro
end
```

## Profundidade na verificação da existência de diretórios

Ao verificar se um diretório existe, é importante ter em mente que o Ruby procura por ele apenas no diretório atual. Isso significa que se você especificar um caminho que começa com `./`, o Ruby irá procurar a partir do diretório em que seu código está sendo executado.

Por exemplo, se seu código está sendo executado na pasta `home/usuario` e você quer verificar a existência do diretório `documentos`, o caminho correto seria `./documentos`. Mas se você escrever apenas `documentos`, o Ruby irá procurar em `home/usuario/documentos`, o que pode resultar em um erro se o diretório estiver em outro local.

Além disso, é importante verificar se o diretório existe antes de realizar qualquer operação nele, pois caso contrário, seu código pode gerar erros desnecessários.

Por fim, é possível utilizar a gem `fileutils` para realizar operações em diretórios de forma mais segura e eficiente. Vale a pena dar uma olhada nos métodos disponíveis nesta gem.

## Veja também
- [Documentação do método `Dir.exist?`](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exists-3F)
- [Documentação do método `File.directory?`](https://ruby-doc.org/core-2.7.1/File.html#method-c-directory-3F)
- [Documentação da gem `fileutils`](https://ruby-doc.org/stdlib-2.7.1/libdoc/fileutils/rdoc/FileUtils.html)