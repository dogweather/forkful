---
title:                "Bash: Buscando e substituindo texto"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

A busca e substituição de texto é uma tarefa comum no mundo da programação, em especial na linguagem Bash. Ao realizar essa ação, é possível agilizar e otimizar o processo de edição de arquivos de texto, poupando tempo e esforço.

## Como fazer

A seguir, apresentaremos alguns exemplos de como realizar a busca e substituição de texto utilizando a linguagem Bash. Esses exemplos podem ser facilmente adaptados para atender às suas necessidades específicas.

Para iniciar, vamos criar um arquivo de texto chamado "lista.txt" com as seguintes informações:

```
1. Maçã
2. Pera
3. Banana
4. Morango
```

Agora, se quisermos substituir a palavra "Pera" por "Abacaxi" em todo o arquivo, podemos utilizar o seguinte comando:

```
sed -i 's/Pera/Abacaxi/g' lista.txt
```

Nesse comando, "sed" é o comando utilizado para fazer a substituição, "-i" é a opção para que a alteração seja feita diretamente no arquivo e 's/Pera/Abacaxi/g' é o padrão de substituição que será aplicado.

Podemos também utilizar a busca e substituição com base em padrões ou expressões regulares. Por exemplo, se quisermos substituir todos os números por asteriscos no arquivo "lista.txt", podemos utilizar o seguinte comando:

```
sed -i 's/[0-9]/*/g' lista.txt
```

Isso irá substituir todos os números, independentemente de quantos dígitos possuem. Ou seja, o número 10 também será substituído por "*".

## Mergulho profundo

A linguagem Bash possui diversas opções e comandos para realizar a busca e substituição de texto de forma mais complexa. Algumas opções comuns são:

- "-i" para fazer a alteração diretamente no arquivo
- "-g" para substituir todas as ocorrências da palavra ou padrão
- "-s" para silenciar as mensagens de erro
- "-r" para utilizar expressões regulares
- "-d" para fazer a alteração sem criar um backup do arquivo original

É importante sempre se certificar de utilizar essas opções de acordo com as suas necessidades e de forma cuidadosa, para evitar qualquer alteração indesejada no seu arquivo.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html#Bourne-Shell-Builtins)
- [Artigo da Linuxaria sobre busca e substituição de texto no Bash](https://linuxaria.com/article/how-to-search-and-replace-text-in-a-file-on-linux-vim-sed-and-more)