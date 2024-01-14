---
title:                "Bash: Criando um arquivo temporário"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que Criar um Arquivo Temporário em Bash

Criar um arquivo temporário é uma prática muito comum em programação em Bash. Essa é uma forma simples e eficiente de armazenar dados temporários que serão utilizados dentro de um script ou programa. Além disso, os arquivos temporários são excluídos automaticamente após o uso, tornando-os uma opção segura para armazenar informações sensíveis.

## Como Criar um Arquivo Temporário em Bash

Criar um arquivo temporário em Bash é muito simples. Você pode utilizar o comando "mktemp" seguido de um nome de arquivo para criar um arquivo temporário.

```Bash
arquivo_temporario=$(mktemp arquivoXXXXXX)
```

Neste exemplo, o "XXXXXX" serve para garantir que o nome do arquivo seja único e não sobrescreva arquivos existentes. Você também pode adicionar uma extensão específica ao nome do arquivo, como ".txt" ou ".sh", dependendo do formato de dados que será armazenado.

Para escrever dados no arquivo temporário, você pode utilizar o comando "echo" seguido do símbolo ">" para especificar o arquivo de destino.

```Bash
echo "Estes são dados temporários" > $arquivo_temporario
```

Você também pode utilizar o comando "cat" para exibir os dados armazenados no arquivo temporário.

```Bash
cat $arquivo_temporario
```

## Mais Informações Sobre a Criação de Arquivos Temporários em Bash

Além disso, é possível criar arquivos temporários com permissões diferentes, definir a quantidade de caracteres no nome do arquivo e especificar um diretório específico para armazenar os arquivos temporários. Para saber mais sobre essas opções, utilize o comando "man mktemp" no seu terminal ou pesquise por "create temporary file in Bash" na internet.

## Veja Também

- [Tutorial: como criar arquivos temporários em Bash](https://www.howtogeek.com/671308/how-to-create-temporary-files-in-bash/)
- [Exemplo de uso de arquivos temporários em Bash](https://linuxhint.com/bash_temporary_file/)
- [Documentação oficial do mktemp](https://man7.org/linux/man-pages/man1/mktemp.1.html)