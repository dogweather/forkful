---
title:                "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por que criar um arquivo temporário em Bash?

Criar um arquivo temporário em Bash pode ser muito útil enquanto se trabalha em um script ou programa. Isso pode ser especialmente útil quando se precisa armazenar informações temporárias, como resultados de cálculos ou dados que serão usados posteriormente. Além disso, a criação de arquivos temporários também é uma boa prática de programação para garantir a limpeza e organização de seus arquivos e diretórios.

## Como criar um arquivo temporário em Bash

Existem algumas maneiras de criar um arquivo temporário em Bash, mas a mais comum é usando o comando `mktemp`. O comando `mktemp` é responsável por criar um arquivo temporário com um nome único e retorna o caminho do arquivo criado. Aqui está um exemplo básico:

```
Bash
# Criando um arquivo temporário
temp_file=$(mktemp)

# Escrevendo algo no arquivo
echo "Este é um exemplo de um arquivo temporário." > $temp_file

# Lendo o conteúdo do arquivo
cat $temp_file

# Saída:
# Este é um exemplo de um arquivo temporário.
```

Além disso, o comando `mktemp` também permite criar arquivos temporários em um diretório específico:

```
Bash
# Criando um arquivo temporário no diretório atual
temp_file=$(mktemp .temp_file.XXXXXXXXXX)

# Escrevendo algo no arquivo
echo "Este é um arquivo temporário criado no diretório atual." > $temp_file

# Lendo o conteúdo do arquivo
cat $temp_file

# Saída:
# Este é um arquivo temporário criado no diretório atual.
```

## Aprofundando na criação de arquivos temporários em Bash

Além do comando `mktemp`, também é possível criar arquivos temporários usando o comando `tempfile`. No entanto, este comando não é tão flexível quanto o `mktemp`, já que não é possível especificar o local do arquivo ou o nome do arquivo. Aqui está um exemplo:

```
Bash
# Criando um arquivo temporário com tempfile
temp_file=$(tempfile)

# Verificando o caminho do arquivo
echo $temp_file

# Saída:
# /tmp/tempfiletxsH68
```

Também é importante lembrar que arquivos temporários criados usando o `mktemp` não são automaticamente excluídos, assim como no exemplo acima. Para garantir a limpeza desses arquivos, é necessário removê-los manualmente ou incluir um comando `rm` no final do script para excluí-los automaticamente.

# Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Guia de referência do Bash](https://linux.die.net/Bash-Beginners-Guide/html/sect_02_06.html)
- [Tutorial de Bash para iniciantes](https://www.hostinger.com.br/tutoriais/tutorial-bash)