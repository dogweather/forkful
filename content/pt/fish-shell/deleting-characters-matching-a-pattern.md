---
title:    "Fish Shell: Excluindo caracteres que correspondem a um padrão"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que
Ao trabalhar com programação, é comum lidar com grandes quantidades de texto e, muitas vezes, precisamos manipulá-lo para obter resultados específicos. Às vezes, pode ser necessário excluir caracteres que correspondam a determinados padrões em um texto. Nesses casos, o uso do Fish Shell pode ser extremamente útil.

## Como Fazer
Para deletar caracteres que correspondem a um padrão específico usando o Fish Shell, podemos usar o comando `string delete` seguido do caractere ou padrão que desejamos excluir. Por exemplo, se quisermos excluir todas as letras maiúsculas de uma string, podemos usar o seguinte código:
```Fish Shell
set texto "Exemplo de Texto"
echo $texto
# Output: Exemplo de Texto
echo $texto | string delete [A-Z]
# Output: xemplo de exto
```
Neste exemplo, usamos o comando `set` para definir a variável `texto` como uma string e, em seguida, imprimimos a string original com o comando `echo`. Em seguida, usamos o comando `echo` novamente, mas agora adicionamos `string delete` e especificamos o padrão `[A-Z]` para excluir todas as letras maiúsculas do texto original.

Podemos usar esse mesmo método para excluir outros padrões específicos, como números, espaços, pontuação ou símbolos. Basta substituir `[A-Z]` pelo padrão desejado.

## Deep Dive
O comando `string delete` usa a linguagem de expressão regulares (regex) para encontrar e excluir padrões em uma string. O regex é uma forma poderosa de manipular strings e pode ser usado para identificar padrões complexos, que podem ser úteis em situações específicas.

Por exemplo, podemos usar o regex para excluir todas as vogais de uma string, em vez de especificar cada uma delas individualmente. Para isso, podemos usar o padrão `aeiou`. Veja o exemplo abaixo:
```Fish Shell
set texto "Exemplo de Texto"
echo $texto
# Output: Exemplo de Texto
echo $texto | string delete [aeiou]
# Output: xmpl d Txt
```

Com o uso do regex, é possível excluir caracteres de uma forma mais dinâmica e eficiente, economizando tempo e esforço.

## Veja também
- [Documentação do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guia de expressões regulares no Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_regex)
- [Tutorial de string manipulation com Fish Shell](https://medium.com/@tbutts/manipulating-strings-like-a-pro-in-fish-shell-9a040ffcf5cf)