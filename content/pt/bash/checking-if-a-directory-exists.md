---
title:    "Bash: Verificando se um diretório existe"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Quando você está escrevendo um script em Bash, é muito comum que você precise verificar se um diretório existe antes de realizar alguma ação. Isso é especialmente importante se o seu script precisa acessar ou criar arquivos nesse diretório. Verificar a existência de um diretório pode ajudar a evitar erros e garantir que seu script funcione corretamente.

## Como Fazer

Para verificar se um diretório existe em um script em Bash, você precisará usar o comando `test` ou `[]` e a opção `-d` para conferir se o caminho especificado é de fato um diretório. Aqui está um exemplo de como isso pode ser feito:

```Bash
if test -d ~/meu_diretorio; then
  echo "O diretório existe"
else
  echo "O diretório não existe"
fi
```

Neste exemplo, estamos usando o comando `test` para verificar se o diretório chamado "meu_diretorio" existe no diretório home do usuário atual. Se o diretório existir, o comando `echo` irá imprimir "O diretório existe", caso contrário, será impresso "O diretório não existe".

Também é possível usar essa mesma lógica dentro de uma estrutura `if-else` usando o operador `[[]]` da seguinte forma:

```Bash
if [[ -d ~/meu_diretorio ]]; then
  echo "O diretório existe"
else
  echo "O diretório não existe"
fi
```

A saída desses comandos pode variar dependendo de cada situação, mas essa é a ideia básica de como verificar a existência de um diretório em Bash.

## Profundidade do Conceito

Ao verificar se um diretório existe em Bash, é importante entender como isso funciona por trás dos bastidores. O comando `test` ou `[]` está verificando se o diretório existe e se é possível acessá-lo. Isso significa que, mesmo que o diretório esteja vazio, o comando ainda considerará como existente.

Também é importante mencionar que essa checagem é feita em relação ao usuário que está executando o script. Ou seja, se o diretório existe, mas o usuário não tem permissão de acesso, o comando irá considerar como "não existe".

Além disso, é possível combinar essa verificação com outros comandos, como `mkdir` para criar o diretório caso ele não exista, ou `cd` para acessá-lo caso ele exista. Assim, a verificação da existência de um diretório se torna uma ferramenta muito útil e versátil em scripts Bash.

## Veja Também

Aqui estão alguns links úteis para aprender mais sobre como verificar a existência de um diretório em Bash:

- [Documentação do comando `test` no Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [Explicações sobre o operador `test` e as opções disponíveis](https://www.cyberciti.biz/faq/unix-linux-shell-find-out-posixfile-is-a-directory-or-not/)
- [Utilizando o operador `[[]]` em scripts Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Constructs.html)

Esperamos que este artigo tenha sido útil para você e que agora você se sinta mais confiante em usar a verificação da existência de diretórios em seus scripts em Bash!