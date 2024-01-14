---
title:    "Gleam: Criando um arquivo temporário"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por que criar um arquivo temporário em Gleam?

Às vezes, em nossos programas Gleam, pode ser necessário criar um arquivo temporário para armazenar dados que serão usados apenas temporariamente. Isso pode ser útil para gerenciar e armazenar informações temporárias sem ter que criar e manter um arquivo permanente.

# Como criar um arquivo temporário em Gleam

Para criar um arquivo temporário em Gleam, você pode usar a função `File.temp_file` e especificar o caminho para o local onde deseja que o arquivo seja criado. Aqui está um exemplo de código:

```Gleam
import File

let temporary_file = File.temp_file("/tmp/")
```

Este código criará um arquivo temporário no diretório `/tmp/` e retornará o caminho para esse arquivo na variável `temporary_file`. Você também pode especificar o nome do arquivo desejado adicionando `/<nome do arquivo>` ao final do caminho. Além disso, você pode usar a função `File.delete` para excluir o arquivo temporário quando não for mais necessário.

# Mergulho profundo

Além de especificar o local e o nome do arquivo, você também pode usar a função `File.temp_file` para definir outras opções, como o tamanho máximo do arquivo e as permissões de acesso. Você pode ver a documentação completa desta função, incluindo todos os seus parâmetros, no site do Gleam.

# Veja também

- Documentação Gleam File: https://gleam.run/documentation/standard-library/file/
- Tutorial Gleam: Criando Arquivos Temporários: https://medium.com/@wlijo/tutorial-gleam-creating-temporary-files-9ff264706503
- Discussão do Reddit sobre a criação de arquivos temporários em Gleam: https://www.reddit.com/r/gleamlang/comments/9dii0p/temporary_files/