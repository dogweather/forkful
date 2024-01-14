---
title:                "Fish Shell: Criar um arquivo temporário"
simple_title:         "Criar um arquivo temporário"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por que criar um arquivo temporário no Fish Shell?

Se você é um programador de scripts em Fish Shell, pode estar se perguntando por que alguém precisaria criar um arquivo temporário em seus códigos. Embora nem sempre seja necessário, há momentos em que essa pode ser uma solução útil. Vamos dar uma olhada em como fazer isso e por que pode ser útil.

## Como criar um arquivo temporário em Fish Shell

Para criar um arquivo temporário em Fish Shell, você pode usar o comando `mktemp` seguido do diretório onde deseja que o arquivo seja criado, como no exemplo abaixo:

```Fish Shell
mktemp -d /caminho/do/diretorio/
```

Isso criará um arquivo temporário no diretório especificado. Você também pode adicionar um prefixo ao nome do arquivo usando a opção `-p`:

```Fish Shell
mktemp -p /caminho/do/diretorio/ prefixo-
```

Além disso, é recomendável usar o modificador de exclusividade `-u` para garantir que o nome do arquivo seja único e não seja sobregravado caso já exista um arquivo com o mesmo nome no mesmo diretório. O comando completo para criar um arquivo temporário com prefixo e exclusividade seria:

```Fish Shell
mktemp -d -p /caminho/do/diretorio/ -u prefixo-
```

Após a execução desses comandos, você verá que um arquivo temporário foi criado no diretório especificado, com um nome como `prefixo-XXXXXX`, onde os "X" são caracteres aleatórios que garantem a exclusividade do arquivo.

## Uma visão mais aprofundada sobre a criação de arquivos temporários

Você pode estar se perguntando por que e quando seria necessário criar um arquivo temporário em seus códigos. Existem alguns casos em que pode ser útil, como por exemplo, quando você precisa armazenar temporariamente informações que serão usadas em algum momento do seu código, mas que não são necessárias o suficiente para serem salvas permanentemente.

Além disso, ao criar um arquivo temporário, você pode especificar o seu formato, tornando-o um meio seguro para armazenar informações confidenciais que você não queira salvar permanentemente no seu sistema.

Por fim, os arquivos temporários também são úteis quando você precisa trabalhar com programas que só aceitam arquivos como entrada, mas você possui apenas as informações em forma de variáveis. Ao criar um arquivo temporário com essas informações, você pode facilitar o uso delas nesses programas.

# Veja também:
- [Documentação oficial do Fish Shell sobre arquivos temporários](https://fishshell.com/docs/current/commands.html#mktemp)
- [Tutorial sobre o uso de arquivos temporários em Fish Shell](https://medium.com/@cleberldsoares/como-usar-arquivos-tempor%C3%A1rios-em-fish-shell-c9edcae2fcf2)
- [Explicação detalhada sobre a criação de arquivos temporários em Shell Script](https://www.shellscript.sh/tips/temporary_files.html)