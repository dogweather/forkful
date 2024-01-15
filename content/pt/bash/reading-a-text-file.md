---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente já se deparou com a necessidade de ler um arquivo de texto durante o desenvolvimento de algum script ou programa em Bash. Ler um arquivo de texto é uma habilidade básica e útil para diversos casos de uso, como lidar com dados armazenados em um arquivo de configuração ou processar informações de um registro de log.

## Como fazer

Para ler um arquivo de texto em Bash, você pode utilizar o comando `read` em conjunto com a sintaxe de redirecionamento de entrada `<` para direcionar o conteúdo de um arquivo para a entrada do comando. Por exemplo, suponha que tenhamos um arquivo chamado `dados.txt` com o seguinte conteúdo:

```
1
2
3
```

Podemos utilizar o seguinte comando em Bash para ler cada linha deste arquivo:

```Bash
# Lendo uma linha por vez
while read linha; do
  echo $linha
done < dados.txt
```

Isso irá imprimir no terminal cada uma das linhas contidas no arquivo `dados.txt`. Também é possível ler um arquivo inteiro de uma vez utilizando o comando `cat` e o redirecionamento de saída, como mostrado abaixo:

```Bash
# Lendo um arquivo inteiro
conteudo=$(cat dados.txt)
echo $conteudo
```

Isso irá armazenar o conteúdo do arquivo `dados.txt` na variável `conteudo` e então imprimi-la no terminal.

## Profundidade

Existem diversas maneiras de ler um arquivo de texto em Bash, além das mostradas anteriormente. Por exemplo, você pode especificar qual o separador a ser usado durante a leitura utilizando a opção `-d` do comando `read`. Além disso, é possível também utilizar o comando `IFS` (Internal Field Separator) para alterar o separador padrão.

Outra funcionalidade interessante é a possibilidade de utilizar o comando `while IFS= read -r linha` para garantir que as quebras de linha do arquivo sejam preservadas durante a leitura. Isso é especialmente útil quando se trabalha com arquivos que possuem espaços em branco no nome ou em seu conteúdo.

Ler um arquivo de texto em Bash também pode ser realizado de forma interativa, permitindo ao usuário inserir dados diretamente no terminal e então salvando-os em um arquivo. Isso agiliza o processo de edição de arquivos de configuração, por exemplo.

## Veja também

- [Documentação oficial do Bash (em inglês)](https://www.gnu.org/software/bash/)
- [Tutorial sobre redirecionamento de entrada e saída em Bash (em inglês)](https://linuxconfig.org/bash-redirections-cheat-sheet)
- [Exemplos práticos de leitura de arquivos em Bash (em inglês)](https://bash.cyberciti.biz/guide/Reads_from_the_file_descriptor_-&lt;age-_&amp;_process_Substitution)