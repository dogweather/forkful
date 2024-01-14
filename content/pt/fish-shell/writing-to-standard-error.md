---
title:    "Fish Shell: Escrevendo para o erro padrão"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que

 Escrever para o erro padrão (standard error) é uma poderosa ferramenta para lidar com erros e depuração em programação. Aprender a utilizar essa função pode ajudar a tornar seu código mais eficiente e resolvente de possíveis problemas.

## Como Fazer

Para escrever para o erro padrão no Fish Shell, você pode utilizar o comando `echo` com a opção `-e` para habilitar sequências de escape. Isso permite que você formate a saída do erro padrão de forma mais clara e legível.

```
Fish Shell Code:
```
echo -e "Houve um erro: \033[31mERRO\033[0m"

```
Output:
```
Houve um erro: ERRO

Além disso, você pode redirecionar o erro padrão para um arquivo de log usando o operador `>` seguido do nome do arquivo. Isso pode ser útil para salvar informações sobre erros específicos e analisá-los posteriormente.

```
Fish Shell Code:
```
ls arquivo_nao_existente > log.txt

```
Output:
```
ls: arquivo_nao_existente: Arquivo ou diretório não encontrado

## Deep Dive

Quando seu código falha, é importante ter informações detalhadas sobre o erro para ajudar a encontrá-lo e corrigi-lo. Ao escrever para o erro padrão, você pode incluir informações como qual parte do código causou o erro, o valor de variáveis importantes e mensagens de erro específicas. Isso pode ser especialmente útil quando seu código é executado em sistemas onde você não tem acesso direto ao terminal.

Além disso, escrever para o erro padrão também pode ajudar a identificar problemas de código relacionados a permissões, cache ou bibliotecas. Ao escrever informações de debug para o erro padrão, você pode analisá-las mais tarde e encontrar a causa raiz do erro.

## Veja Também

Aprender a escrever para o erro padrão pode ser uma habilidade valiosa para programadores em qualquer nível de experiência. Para mais informações e dicas sobre como utilizar essa função, confira os links abaixo:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Exemplo de uso do `echo` com o Fish Shell](https://fishshell.com/docs/current/cmds/echo.html)
- [Como redirecionar a saída do erro padrão no Fish Shell](https://unix.stackexchange.com/questions/237980/how-to-redirect-stderr-in-fish)
- [Como formatar a saída do erro padrão no Fish Shell](https://unix.stackexchange.com/questions/87824/how-can-i-redirect-and-append-both-stdout-and-stderr-to-a-file-with-bash)