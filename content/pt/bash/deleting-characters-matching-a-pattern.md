---
title:                "Bash: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Existem muitas situações em que se pode precisar excluir caracteres que correspondam a um determinado padrão em um arquivo ou texto. Isso pode ser necessário para limpar dados, formatar um documento ou até mesmo para criar um programa mais eficiente. Qualquer que seja o motivo, é importante saber como realizar essa tarefa no Bash.

## Como fazer

A exclusão de caracteres correspondentes a um padrão pode ser feita usando o comando `sed` no Bash. Este comando é usado para realizar operações de edição em um arquivo ou texto. Para excluir caracteres correspondentes a um padrão, basta seguir a seguinte sintaxe:

```
sed 's/padrão//' arquivo.txt
```

Neste exemplo, o caractere `/` é usado para separar o padrão do texto que deve ser editado. O `sed` irá procurar pelo padrão indicado e excluir todos os caracteres correspondentes dentro do arquivo indicado. Também é possível usar expressões regulares para indicar padrões mais complexos.

Por exemplo, se quisermos excluir todas as vogais maiúsculas de um texto, podemos usar a seguinte sintaxe:

```
sed 's/[AEIOU]//g' arquivo.txt
```

Neste caso, a flag `g` é adicionada no final para indicar que o comando deve procurar por todas as ocorrências do padrão dentro do arquivo. Vale ressaltar que o comando `sed` não altera o arquivo original, mas sim imprime o resultado da edição na saída padrão. Para salvar as alterações, é preciso redirecionar a saída para um novo arquivo, como por exemplo `sed 's/[AEIOU]//g' arquivo.txt > novo_arquivo.txt`.

## Mergulho profundo

Para ser mais preciso nos padrões que devem ser removidos, também é possível usar a opção `-i` com o comando `sed`. Isso permite que as alterações sejam feitas diretamente no arquivo original, sem ser necessário redirecionar a saída.

Além disso, o `sed` também possui outras opções e funcionalidades, como a possibilidade de substituir caracteres correspondentes por outros, usar grupos de captura e outros recursos. Para saber mais detalhes e exemplos de uso, consulte a documentação oficial do comando.

## Veja também

Aqui estão alguns links úteis para continuar aprendendo sobre como deletar caracteres correspondentes a um padrão no Bash:

- Documentação oficial do comando `sed`: https://www.gnu.org/software/sed/manual/sed.html
- Tutorial de expressões regulares do Bash: https://linux.die.net/Bash-Beginners-Guide/sect_04_01.html
- Mais exemplos práticos do uso do `sed`: https://www.tecmint.com/linux-sed-command-to-delete-lines-in-file/