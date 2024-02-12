---
title:                "Verificando se um diretório existe"
aliases: - /pt/elixir/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:03.601750-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verificando se um diretório existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Verificar se um diretório existe em Elixir trata de confirmar a presença de um diretório em um caminho especificado no sistema de arquivos. Programadores fazem isso para garantir que eles possam ler, escrever ou realizar operações no diretório com segurança, sem encontrar erros devido à sua ausência.

## Como fazer:
A biblioteca padrão do Elixir oferece uma maneira direta de verificar a existência de um diretório por meio do módulo `File`. Veja como você pode usá-lo:

```elixir
if File.dir?("caminho/para/diretorio") do
  IO.puts "Diretório existe!"
else
  IO.puts "Diretório não existe."
end
```

Saída de exemplo, assumindo que o diretório não exista:
```
Diretório não existe.
```

Para interações mais avançadas com o sistema de arquivos, incluindo a verificação da existência de diretórios, você pode considerar o uso de bibliotecas de terceiros como `FileSystem`. Embora as capacidades padrão do Elixir sejam suficientes para muitos casos, `FileSystem` pode oferecer um controle mais matizado e feedback para aplicações complexas. No entanto, para a necessidade básica de verificar se um diretório existe, recomenda-se geralmente aderir ao módulo `File` nativo, já que está prontamente disponível e não requer quaisquer dependências externas.
