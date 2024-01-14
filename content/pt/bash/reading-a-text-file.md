---
title:    "Bash: Lendo um arquivo de texto"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Bash?

Se você é novo na programação em Bash, ou mesmo se já está familiarizado com a linguagem, pode se perguntar por que alguém gostaria de ler um arquivo de texto em um script. A resposta é simples: a leitura de arquivos de texto é uma tarefa comum na programação e pode ser útil para acessar dados armazenados em um formato legível por humanos.

## Como fazer

Para ler um arquivo de texto em Bash, é necessário utilizar o comando `read` em conjunto com a estrutura `while` para iterar sobre as linhas do arquivo. O código a seguir mostra um exemplo de como fazer isso:

```Bash
#!/bin/bash

# Abrir e ler arquivo
while read linha
do
    echo "$linha" # Imprime cada linha do arquivo
done < arquivo.txt # Substitua "arquivo.txt" pelo nome do seu arquivo

```

O comando `read` é usado para ler a entrada do usuário, mas também pode ser utilizado para ler arquivos. Em nosso exemplo, definimos a variável `linha` como sendo a entrada lida pelo `while`, e então a imprimimos usando o comando `echo`. O `< arquivo.txt` indica qual arquivo será lido pelo script.

## Mergulhando mais fundo

Ao ler um arquivo de texto, é importante ter em mente que o script irá percorrer o arquivo linha por linha. Isso significa que qualquer manipulação de dados ou lógica deve ser aplicada dentro da estrutura `while` para cada linha individualmente.

Além disso, é possível utilizar o comando `read` para ler valores específicos em cada linha, dividindo-os por meio de um delimitador. Por exemplo, se quisermos ler um arquivo CSV com valores separados por vírgulas, podemos usar o comando `IFS` para definir a vírgula como delimitador:

```Bash
#!/bin/bash

# Abrir e ler arquivo CSV
while IFS=',' read coluna1 coluna2 coluna3 # Define as colunas separadas por vírgulas
do
    # Faz algo com os valores lidos
    echo "Nome: $coluna1"
    echo "Sobrenome: $coluna2"
    echo "Idade: $coluna3"
done < arquivo.csv # Substitua "arquivo.csv" pelo nome do seu arquivo

```

Neste caso, cada valor separado por vírgula será armazenado em sua respectiva variável, e podemos usar esses valores para realizar alguma ação específica no script.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Artigo sobre a leitura de arquivos em Bash no site Medium](https://medium.com/@LordGhostX/how-to-read-from-a-file-using-bash-a0b52a32d4ba)
- [Vídeo tutorial sobre o uso do comando `read` em Bash](https://www.youtube.com/watch?v=SEqu5gHct-I)