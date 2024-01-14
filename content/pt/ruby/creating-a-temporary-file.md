---
title:    "Ruby: Criando um arquivo temporário."
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Ruby?

Criar arquivos temporários é uma prática comum em vários tipos de programação, inclusive em Ruby. Eles são usados para armazenar dados temporariamente durante a execução de um programa e são úteis para diversas tarefas, como cache de informações e compartilhamento de dados entre diferentes scripts.

## Como criar um arquivo temporário em Ruby

Para criar um arquivo temporário em Ruby, podemos utilizar a classe `Tempfile` do Ruby Standard Library. Ela possui métodos que nos permitem criar, abrir e manipular arquivos temporários de forma simples e segura.

Vamos ver um exemplo de código para criar um arquivo temporário em Ruby:

```Ruby
require 'tempfile'

tempfile = Tempfile.new('arquivo_temporario')
```

Ao executar esse código, um arquivo temporário será criado com o nome "arquivo_temporario". Podemos adicionar conteúdo ao arquivo utilizando o método `puts` e podemos ler seu conteúdo utilizando o método `read`:

```Ruby
tempfile.puts "Olá, mundo!"
tempfile.puts "Esse é um arquivo temporário."
puts tempfile.read
```

A saída desse código será:

```
Olá, mundo!
Esse é um arquivo temporário.
```

Após finalizar as operações com o arquivo, é importante fechá-lo utilizando o método `close`. Isso garante que o arquivo seja excluído automaticamente depois que o programa terminar sua execução.

## Profundidade em criar arquivos temporários

Criar arquivos temporários pode ser uma tarefa simples, mas existem certos detalhes que precisamos levar em consideração para garantir que tudo funcione corretamente e de forma segura. Alguns desses detalhes incluem:

- Especificar o prefixo e sufixo do nome do arquivo temporário: podemos utilizar os argumentos `prefix` e `suffix` ao criar um arquivo temporário para especificar o início e o final do nome do arquivo, respectivamente.
- Especificar o diretório para criação do arquivo: se quisermos que o arquivo temporário seja criado em um diretório específico, podemos utilizar o argumento `dir` ao criar o arquivo.
- Utilizar blocos para garantir que o arquivo seja fechado corretamente: ao invés de utilizar o método `close` explicitamente, podemos utilizar um bloco de código para garantir que o arquivo seja fechado automaticamente depois que o bloco terminar a execução.
- Utilizar o método `unlink` para excluir o arquivo manualmente: em algumas situações, pode ser necessário excluir o arquivo temporário manualmente antes que o programa termine sua execução. Para isso, podemos utilizar o método `unlink`.

Com todas essas informações, estamos prontos para utilizar arquivos temporários em nossos programas em Ruby de forma segura e eficiente.

## Veja também

- Documentação oficial da classe `Tempfile`: https://ruby-doc.org/stdlib-2.7.2/libdoc/tempfile/rdoc/Tempfile.html
- Tutorial sobre manipulação de arquivos em Ruby: https://www.rubyguides.com/2015/05/working-with-files-ruby/