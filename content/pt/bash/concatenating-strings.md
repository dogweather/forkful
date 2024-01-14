---
title:    "Bash: Unindo strings"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que?

Concatenar strings é uma parte importante da linguagem de programação Bash e é usada para combinar múltiplas strings em uma única string. Essa habilidade pode ser muito útil ao escrever scripts ou programas em Bash, especialmente quando se lida com entrada de dados ou impressão de saída.

## Como fazer

Para concatenar strings em Bash, usamos o operador `+` ou um ponto para unir as strings. Vamos dar uma olhada em alguns exemplos de código:

```Bash
# Concatenando duas strings usando o operador +
string1="Olá"
string2="mundo"
string3=$string1" "$string2 # Note o espaço entre as aspas
echo $string3 # Saída: Olá mundo

# Concatenando duas strings usando um ponto
nome="Maria"
sobrenome="Silva"
nomecompleto=$nome.$sobrenome # Note a falta de espaço entre as aspas
echo $nomecompleto # Saída: Maria.Silva
```

Também podemos usar o formato `${string1}${string2}` para concatenar strings e especificar uma ordem específica.

**Nota:** É importante notar que a ordem de concatenar as strings é importante. Ao usar o operador `+`, a primeira string especificada será a primeira na saída, enquanto ao usar um ponto, a última string especificada será a primeira na saída.

## Mergulho profundo

Existem algumas outras coisas que devemos ter em mente ao concatenar strings em Bash:

- As strings não podem conter espaços em branco ao redor do operador `+` ou do ponto.
- Podemos usar variáveis vazias para concatenar uma string simplesmente deixando um lado do operador em branco, como em `echo $string1" "$string2`.
- Podemos usar diferentes tipos de aspas para criar e imprimir strings, mas é preciso ficar atento às diferenças de comportamento. As aspas simples, por exemplo, ignoram variáveis e comandos dentro delas, enquanto as aspas duplas expandem essas variáveis e comandos.
- Podemos também concatenar variáveis com strings fixas, como em `echo "Bem-vindo, "$nome`.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial de concatenação de strings em Bash](https://www.shell-tips.com/bash/string-concatenation-in-bash/)
- [Exemplos práticos de concatenação de strings em Bash](https://www.cyberciti.biz/faq/bash-string-concatenation/)