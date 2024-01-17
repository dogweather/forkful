---
title:                "Verificando se um diretório existe"
html_title:           "Elixir: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

O que é e por que?

Verificar se um diretório existe é um processo importante na programação. É uma maneira de determinar se um determinado diretório está disponível no sistema ou não. Os programadores fazem isso para garantir que seu código possa funcionar corretamente com base na presença ou ausência de um diretório específico.

Como fazer?

```Elixir
def check_directory(directory) 
  exists = File.dir?(directory)
  if exists do 
    IO.puts("O diretório #{directory} existe.")
  else 
    IO.puts("O diretório #{directory} não existe.")
  end
end
```

Saída do exemplo:

```
O diretório existe/dir existe.
```

Aprofundando mais

Verificar a existência de um diretório não é um conceito novo e pode ser encontrado em outras linguagens de programação. No entanto, em Elixir, podemos usar a função `File.dir?` para verificar se um diretório existe ou não. Alternativamente, também podemos usar a função `File.stat` para obter mais informações sobre um diretório, como tamanho, dono, permissões etc.

Ver também

Documentação do Elixir para `File.dir?`: https://hexdocs.pm/elixir/File.html#dir?/1