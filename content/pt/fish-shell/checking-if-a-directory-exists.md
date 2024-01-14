---
title:    "Fish Shell: Verificando a existência de um diretório"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que verificar a existência de um diretório?

Ao trabalhar com Fish Shell, muitas vezes é necessário verificar se um diretório existe antes de fazer qualquer operação com ele. Isso pode ser útil para garantir que o diretório esteja presente antes de tentar acessá-lo ou para evitar erros no código ao tentar interagir com um diretório inexistente. Neste artigo, vamos explorar como fazer isso de forma simples e eficiente.

## Como fazer

Para verificar se um diretório existe usando o Fish Shell, podemos usar o comando `test -d`. Este comando retorna verdadeiro (ou seja, 0) se o diretório fornecido existir e falso (ou seja, um valor diferente de 0) se o diretório não existir. Vamos dar uma olhada em um exemplo:

```Fish Shell
test -d ~/Documents
echo $status
```

O comando `test -d` é usado para verificar se o diretório `~/Documents` existe. Se existir, o status será 0 e o comando `echo` imprimirá esse status no console. Caso contrário, o status será diferente de 0 e nada será impresso no console.

Outra forma de realizar essa verificação é usando as estruturas de controle `if` e `else`. Vamos ver como isso pode ser feito:

```Fish Shell
if test -d ~/Documents
	echo "O diretório existe!"
else
	echo "O diretório não existe!"
end
```

Neste exemplo, usamos o comando `test -d` dentro de uma estrutura de controle `if`. Se o diretório existir, o comando `echo` será executado e "O diretório existe!" será impresso. Caso contrário, o comando `echo` dentro da estrutura `else` será executado e "O diretório não existe!" será impresso.

## Mergulho profundo

Você pode estar se perguntando: por que usar o comando `test -d` em vez de simplesmente tentar acessar o diretório e verificar se ocorreu um erro? A resposta é que isso pode economizar tempo e processamento. Ao usar o `test -d`, o Fish Shell realiza apenas uma verificação rápida e retorna um status que indica a existência ou não do diretório. Se tentarmos acessar o diretório e ele não existir, isso resultará em uma falha e mais tempo de processamento será perdido.

Além disso, você também pode usar outras opções com o comando `test -d`, como `test -w` para verificar se o diretório é gravável ou `test -r` para verificar se o diretório é legível.

## Veja também

- Documentação do Fish Shell sobre o comando `test`: https://fishshell.com/docs/current/cmds/test.html
- Mais informações sobre estruturas de controle no Fish Shell: https://fishshell.com/docs/current/scripting.html#introscripting
- Exemplos de uso do `test` em scripts: https://stackoverflow.com/questions/8803360/test-d-what-does-it-do