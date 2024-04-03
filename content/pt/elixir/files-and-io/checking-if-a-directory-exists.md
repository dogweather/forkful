---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:03.601750-07:00
description: "Como fazer: A biblioteca padr\xE3o do Elixir oferece uma maneira direta\
  \ de verificar a exist\xEAncia de um diret\xF3rio por meio do m\xF3dulo `File`.\
  \ Veja como voc\xEA\u2026"
lastmod: '2024-03-13T22:44:46.252391-06:00'
model: gpt-4-0125-preview
summary: "A biblioteca padr\xE3o do Elixir oferece uma maneira direta de verificar\
  \ a exist\xEAncia de um diret\xF3rio por meio do m\xF3dulo `File`."
title: "Verificando se um diret\xF3rio existe"
weight: 20
---

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
