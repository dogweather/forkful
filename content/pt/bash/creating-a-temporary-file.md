---
title:    "Bash: Gerando um arquivo temporário"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Bash?

Existem várias razões pelas quais alguém pode criar um arquivo temporário em um programa Bash. Um dos motivos mais comuns é quando precisamos armazenar dados temporários para serem utilizados posteriormente, mas não queremos salvar esses dados permanentemente em um arquivo. Criar um arquivo temporário nos permite armazenar esses dados de forma rápida e prática, sem afetar arquivos existentes ou ocupar espaço desnecessário no sistema.

## Como criar um arquivo temporário em Bash

Para criar um arquivo temporário em Bash, utilizamos o comando `mktemp`. Veja abaixo um exemplo de código:

```Bash
# Criando um arquivo temporário
temp_file=$(mktemp)

# Escrevendo no arquivo temporário
echo "Olá! Este é um arquivo temporário." > $temp_file

# Lendo o conteúdo do arquivo temporário
cat $temp_file
```

O resultado da execução desse código seria:

```Bash
Olá! Este é um arquivo temporário.
```

## Mergulho profundo

O comando `mktemp` é responsável por criar um arquivo temporário de forma segura, ou seja, garantindo que o arquivo tenha um nome único. Isso é importante porque evita conflitos caso dois programas estejam criando arquivos temporários ao mesmo tempo. Além disso, é possível especificar um padrão de nome para o arquivo temporário utilizando a opção `-p` e escolher o diretório onde o arquivo será criado.

Também é importante mencionar que, por padrão, o arquivo temporário criado terá permissões de leitura e escrita para o usuário que executou o comando. No entanto, é possível modificar essas permissões utilizando a opção `-m` seguida pelos dígitos correspondentes às permissões desejadas.

## Veja também

- Documentação oficial do comando `mktemp`: https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html#mktemp-invocation
- Tutorial sobre como criar arquivos temporários em Bash: https://medium.com/@amirhidar/how-to-create-a-temporary-file-in-bash-6a97f016be44
- Exemplos de uso do `mktemp`: https://bash.cyberciti.biz/file-management/how-to-create-temporary-file/

Agora que você aprendeu como criar um arquivo temporário em Bash, pode utilizá-lo para armazenar informações temporárias em seus projetos! Lembre-se sempre de utilizar o comando `mktemp` de forma adequada e segura.