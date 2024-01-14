---
title:    "Bash: Criando um arquivo temporário"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar arquivos temporários é importante?

Muitas vezes, ao escrever códigos em Bash, é necessário lidar com arquivos temporários. Esses arquivos são criados para armazenar dados temporários que podem ser usados ​​durante a execução de um programa. A criação de arquivos temporários é uma técnica comum e útil que pode ser usada em uma variedade de situações, como manipulação de dados, geração de relatórios e muito mais.

## Como criar um arquivo temporário em Bash

Criar um arquivo temporário em Bash pode ser facilmente feito usando o comando "touch" seguido pelo nome do arquivo que você deseja criar. Por exemplo, se você quiser criar um arquivo chamado "temp.txt", basta digitar ```Bash
touch temp.txt
```
Isso criará um arquivo temporário vazio com o nome "temp.txt" em seu diretório atual.

Outra maneira de criar um arquivo temporário é usando o comando "mktemp". Este comando é mais versátil, pois permite especificar o nome do arquivo e o diretório onde ele será criado. Por exemplo, para criar um arquivo chamado "temp.txt" no diretório atual, você pode usar o seguinte comando: ```Bash
mktemp temp.txt
```

## Aprofundando: Como funcionam os arquivos temporários em Bash?

Os arquivos temporários criados em Bash têm algumas características importantes a serem observadas. Primeiro, eles são criados com permissões restritas, o que significa que apenas o usuário atual tem permissão para ler e escrever neles. Isso é feito para garantir que apenas o programa que criou o arquivo possa acessá-lo, evitando assim a possibilidade de interferência de outros programas.

Além disso, os arquivos temporários são excluídos automaticamente quando o programa é encerrado, a menos que você especifique o contrário. Isso é útil para evitar a poluição do sistema com arquivos desnecessários. No entanto, é importante lembrar que, se o programa falhar antes de excluir o arquivo temporário, ele será mantido no sistema.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Usando arquivos temporários em Bash](https://tldp.org/LDP/abs/html/tempfiles.html)
- [Conceitos básicos de Bash para iniciantes](https://linux.die.net/Bash-Beginners-Guide/)