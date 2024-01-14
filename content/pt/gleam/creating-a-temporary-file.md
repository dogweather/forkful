---
title:    "Gleam: Criando um arquivo temporário"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Gleam?

Às vezes, ao desenvolver um aplicativo em Gleam, é necessário armazenar dados temporários que serão usados durante a execução do programa. Nesses casos, criar um arquivo temporário pode ser a solução ideal.

## Como criar um arquivo temporário em Gleam

Criar um arquivo temporário em Gleam é um processo simples e direto. Basta seguir os passos abaixo:

1. Importe o módulo `gleam/file`.

```Gleam
import gleam/file
```

2. Utilize a função `file.tmp(path)`, onde `path` é o caminho onde o arquivo temporário deve ser criado. Por exemplo, se quiser criar um arquivo temporário na pasta `temp`, o código ficaria assim:

```Gleam
file.tmp("temp")
```

3. O retorno dessa função será uma estrutura de dados `File`, que pode ser utilizada para escrever ou ler conteúdo no arquivo temporário.

4. Ao final do processo, é importante fechar o arquivo temporário utilizando `file.close(temp_file)`, onde `temp_file` é o nome da estrutura de dados `File` criada anteriormente.

## Aprofundando-se na criação de arquivos temporários em Gleam

Criar um arquivo temporário em Gleam pode ser mais útil do que apenas armazenar dados temporários do programa. É possível, por exemplo, utilizá-lo para armazenar logs de execução, criar backups de informações importantes, entre outras possibilidades.

Além disso, ao criar um arquivo temporário, é possível definir algumas configurações adicionais, como a extensão do arquivo, permissões de leitura e escrita, entre outras opções. Essas configurações podem ser definidas utilizando a função `file.tmp_with_options()`.

## Veja também

- [Documentação do módulo `gleam/file`](https://gleam.run/docs/stdlib/file)
- [Exemplos de uso de arquivos temporários em Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/temp_file.gleam)