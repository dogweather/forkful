---
title:    "Bash: Lendo argumentos da linha de comando"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Se você é um programador iniciante, pode se perguntar por que precisa se preocupar com a leitura de argumentos da linha de comando em programas Bash. A resposta é simples: isso permite que o usuário forneça informações ao programa e personalize sua execução de acordo com suas necessidades específicas. A habilidade de ler argumentos da linha de comando é um importante aspecto da programação em Bash e é essencial para a criação de programas mais interativos e versáteis.

## Como ler argumentos da linha de comando

A leitura de argumentos da linha de comando é feita por meio do uso de parâmetros especiais que são passados ​​para o programa quando ele é executado. Esses parâmetros são acessados ​​usando a variável especial `$n`, onde `n` é o número do argumento desejado. Por exemplo, se você deseja acessar o primeiro argumento passado para o seu programa, use `$1`. Você também pode usar `shift` para percorrer os argumentos um por um.

Aqui está um exemplo de código que lê os argumentos da linha de comando e imprime seu valor:

​```Bash
#!/bin/bash
echo "O primeiro argumento é: $1"
echo "O segundo argumento é: $2"
echo "O terceiro argumento é: $3"
```

**Exemplo de saída:**

```
$ ./programa.sh primeiro segundo terceiro
O primeiro argumento é: primeiro
O segundo argumento é: segundo
O terceiro argumento é: terceiro
```

## Mergulho Profundo

A leitura de argumentos da linha de comando pode ser um pouco mais complicada do que parece à primeira vista. Além de usar o `shift` para percorrer os argumentos um por um, você também pode optar por usar um loop `while` para percorrer todos os argumentos.

Além disso, você também pode usar as opções `getopts` para lidar com argumentos opcionais e argumentos com valores. Isso permite que você crie programas mais robustos e completos que podem processar uma variedade maior de argumentos da linha de comando.

## Veja também

Aqui estão alguns links úteis para mais informações sobre como ler argumentos da linha de comando em Bash:

- [Guia rápido sobre argumentos da linha de comando no Linux](https://www.linode.com/docs/tools-reference/linux-command-line/arguments)
- [Documentação oficial do Bash sobre argumentos da linha de comando](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#Bash-Variables)
- [Guia de uso de opções `getopts` em programas Bash](https://stackoverflow.com/questions/16483119/an-unofficial-bash-internal-getopts-tutorial)