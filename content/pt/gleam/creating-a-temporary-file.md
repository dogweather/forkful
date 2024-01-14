---
title:                "Gleam: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Gleam?

Existem várias razões pelas quais um programador pode querer criar um arquivo temporário ao trabalhar com Gleam. Uma delas é quando precisamos armazenar dados temporariamente durante a execução de um programa. Isso pode ser útil, por exemplo, quando precisamos criar um arquivo de log ou quando estamos manipulando dados em um formato específico por um curto período de tempo.

Outra razão comum é quando estamos trabalhando em projetos que exigem a geração de arquivos temporários, como é o caso de testes ou ensaios de desempenho.

Felizmente, criar um arquivo temporário em Gleam é uma tarefa bastante simples. Na seção a seguir, vamos mostrar como fazer isso passo a passo.

## Como criar um arquivo temporário em Gleam?

Para criar um arquivo temporário em Gleam, precisamos utilizar a biblioteca `standard/file` e sua função `temporary_file`. Esta função recebe dois argumentos: o primeiro é o prefixo de nome do arquivo temporário, e o segundo é a extensão do arquivo.

Veja um exemplo de código abaixo, que cria um arquivo temporário com o prefixo `temp` e extensão `.txt`:

```Gleam
import standard/file

let { path, writer } = file.temporary_file("temp", ".txt")
```

Ao executar este código, o arquivo temporário será criado e o caminho do arquivo será armazenado na variável `path`. Note que este arquivo só será criado na pasta que o seu programa estiver sendo executado. Se você quiser especificar um diretório diferente, é possível passar um terceiro argumento opcional com o caminho desejado.

Além disso, com a variável `writer`, podemos escrever conteúdo no arquivo temporário utilizando a função `write` após sua criação. Podemos também utilizar outras funções da biblioteca `file` para manipular este arquivo temporário, como por exemplo, renomeá-lo ou deletá-lo.

## Aprofundando-se na criação de arquivos temporários em Gleam

Apesar de ser uma tarefa simples, é importante destacar que a criação de arquivos temporários em Gleam segue uma lógica específica. Devemos sempre abrir o arquivo temporário, escrever ou manipular seu conteúdo e, ao final, fechá-lo para garantir que ele seja criado corretamente. Caso contrário, podemos ter erros durante a execução do programa.

Além disso, é também importante lembrar de realizar a limpeza dos arquivos temporários criados após o término do seu uso, para evitar ocupação desnecessária de espaço em disco.

## Veja também

- Documentação da biblioteca `standard/file`: https://gleam.run/modules/standard_file.html
- Tutorial básico de Gleam: https://iexiaorui.github.io/tutorial-gleam/