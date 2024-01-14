---
title:                "Bash: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Porque

Se você é um programador iniciante ou até mesmo experiente, pode se deparar com a necessidade de verificar se um diretório específico existe. Ao fazer isso, você pode economizar tempo e evitar quaisquer erros causados pela tentativa de acessar um diretório inexistente. Com o Bash, há uma maneira fácil de verificar se um diretório existe antes de executar seu código. Neste artigo, vamos dar um mergulho profundo nesse processo e ajudá-lo a entender por que isso é importante.

## Como Fazer

Existem duas maneiras comuns de verificar se um diretório existe com Bash: usando o comando `ls` ou o comando `test`. Aqui está um exemplo de cada:

```Bash
if ls ~/meu-diretorio > /dev/null 2>&1; then
  echo "O diretório existe!"
else
  echo "O diretório não existe."
fi
```

```Bash
if test -d ~/meu-diretorio; then
  echo "O diretório existe!"
else
  echo "O diretório não existe."
fi
```

Vamos quebrar esses comandos para entender o que está acontecendo. Primeiro, ambos os comandos usam a condicional `if` para determinar se o diretório existe ou não. Em seguida, temos o comando `ls` seguido de `~/meu-diretorio`. Isso tentará listar o conteúdo do diretório, mas especificamos que a saída seja redirecionada para o arquivo `/dev/null` e quaisquer erros sejam redirecionados para o mesmo local. Isso significa que não veremos nenhuma saída ou erro no terminal quando o comando for executado. Se o comando for bem-sucedido, isso significa que o diretório existe e a mensagem "O diretório existe!" será exibida. Caso contrário, a mensagem "O diretório não existe." será exibida. 

No segundo exemplo, usamos o comando `test` com a opção `-d`, que verifica se o caminho especificado é um diretório. Neste caso, o caminho é `~/meu-diretorio`. Se o teste for verdadeiro, a mensagem "O diretório existe!" será exibida. Caso contrário, a mensagem "O diretório não existe." será exibida.

Agora que você sabe como verificar se um diretório existe com Bash, vamos dar um mergulho mais profundo para entender melhor por que esse processo é importante.

## Mergulho Profundo

Ao verificar se um diretório existe antes de executar seu código, você pode evitar erros e problemas em sua aplicação. Se o diretório não existir e seu código tentar acessá-lo, isso pode resultar em um erro que interrompa a execução do programa. Isso pode ser especialmente problemático se o diretório for necessário para o funcionamento correto do seu código.

Além disso, verificar a existência de um diretório também pode ser útil para lidar com opções ou caminhos de diretórios fornecidos pelo usuário. Se um usuário fornecer um caminho inválido ou inexistente, seu código pode lidar com isso de forma adequada ao verificar primeiro se o diretório existe.

É importante notar que, embora o `test` e o `ls` sejam os métodos mais comuns para verificar a existência de um diretório, existem outras maneiras de realizar essa tarefa, como usando o comando `find` ou `stat`. Cada método pode ter suas próprias vantagens ou desvantagens, portanto, é sempre bom estar ciente de suas opções e escolher o melhor método para a sua situação específica.

## See Also

Aqui estão alguns links úteis para ajudá-lo a continuar explorando o processo de verificação de existência de diretório com Bash:

- [Documentação Bash do Linux](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [Bash Beginner's Guide](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/index.html)
- [Existe um diretório?](https://www.cyberciti.biz/tips/find-out-if-directory-exists.html)