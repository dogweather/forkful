---
title:                "Bash: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Por que escrever para o erro padrão no Bash

Se você já programou em Bash, provavelmente já se deparou com o uso do comando "echo" para imprimir mensagens na tela. No entanto, nem sempre queremos que essas mensagens sejam impressas no terminal padrão. É aqui que entra o uso de escrever para o erro padrão, permitindo que mensagens importantes sejam exibidas durante a execução do programa, mesmo se a saída padrão estiver sendo redirecionada ou suprimida.

## Como fazer

A forma mais comum de escrever para o erro padrão em Bash é utilizando o comando ">&2" após a mensagem que se deseja imprimir. Por exemplo, se quisermos imprimir uma mensagem de erro, podemos usar o seguinte código:

```Bash
echo "Erro: o arquivo não pode ser encontrado!" >&2
```

Isso garantirá que a mensagem seja impressa no erro padrão, tornando-a mais visível e garantindo que o usuário a veja, independente da saída padrão.

## Deep Dive

Para entender melhor como funciona a escrita para o erro padrão no Bash, precisamos primeiro entender um pouco sobre os descritores de arquivo. Todo arquivo aberto em um sistema operacional possui um número exclusivo associado a ele, chamado de descritor de arquivo. O descritor de arquivo 1 é o padrão para a saída do terminal, enquanto o descritor de arquivo 2 é o padrão para o erro padrão.

Ao usar o comando ">&2", estamos redirecionando a saída para o descritor de arquivo 2, ou seja, o erro padrão. Isso garante que a mensagem seja tratada como um erro pelo Bash e seja impressa no terminal, mesmo que a saída padrão esteja sendo redirecionada ou suprimida.

Além disso, é importante lembrar que o Bash possui um comando "exec" que pode mudar os descritores de arquivo e permitir que a saída padrão seja redirecionada para o erro padrão e vice-versa. Isso pode ser útil em casos específicos, mas requer cuidado ao ser utilizado.

## Veja também

- Guia de redirecionamento de saída e erro em Bash: https://linuxhint.com/bash_output_redirection_guide/
- Compreendendo o comando "exec" em Bash: https://www.shell-tips.com/bash/exec/
- Documentação oficial do Bash (em português): https://www.gnu.org/software/bash/manual/html_node/index.html#Top