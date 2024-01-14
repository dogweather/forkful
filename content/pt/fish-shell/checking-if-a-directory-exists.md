---
title:    "Fish Shell: Verificando se um diretório existe"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que
Você já se deparou com a situação de precisar verificar se um diretório existe antes de executar um comando ou script em seu terminal? Se sim, você já sabe o quão útil essa tarefa pode ser para garantir que seus comandos funcionem corretamente. Neste artigo, vamos mostrar como você pode facilmente verificar a existência de um diretório usando o Fish Shell.

## Como fazer
Para verificar se um diretório existe em seu sistema, podemos usar o comando `test -d` e fornecer o caminho do diretório que queremos verificar. Por exemplo, se queremos verificar se existe um diretório chamado "imagens" dentro da nossa pasta de usuário, podemos usar o seguinte comando:

```
test -d ~/imagens
```

Se o diretório existir, o comando retornará verdadeiro e nada será impresso no terminal. Caso contrário, ele retornará falso e você verá uma mensagem de erro informando que o diretório não pode ser acessado. 

Além disso, podemos usar a opção `-q` para tornar o comando "silencioso", o que significa que ele não imprimirá nada no terminal, mesmo se o diretório existir. Isso pode ser útil quando queremos usar essa verificação em um script e não queremos que o terminal seja poluído por mensagens desnecessárias.

Um exemplo completo de como verificar se um diretório existe e exibir uma mensagem apropriada pode ser:

```
if test -d ~/imagens -q
    echo "O diretório de imagens existe!"
else
    echo "O diretório de imagens não existe."
end
```

## Análise mais detalhada
Agora vamos fazer uma análise mais profunda sobre como o comando `test -d` funciona. O `test` é um comando embutido no Fish Shell que é usado para avaliar uma expressão ou condição e retornar verdadeiro ou falso. Já o `-d` é a opção específica para verificar a existência de um diretório. Ele pode ser combinado com outras opções, como `-e` para verificar a existência de um arquivo ou `-s` para verificar se o arquivo ou diretório não está vazio.

Em termos de retorno, o comando `test -d` retornará 0 (verdadeiro) se o diretório existir e 1 (falso) se ele não existir ou se houver algum problema ao tentar acessá-lo. Além disso, o comando também respeita as permissões do diretório e só retornará verdadeiro se o usuário atual tiver permissão para acessá-lo.

Portanto, se você quiser garantir que um diretório exista antes de executar um comando, certifique-se de usar o comando `test -d` e verificar o retorno antes de prosseguir com sua execução.

## Veja também
- Documentação oficial do Fish Shell: https://fishshell.com/docs/current/cmds/test.html
- Tutoriais sobre uso de variáveis no Fish Shell: https://fishshell.com/docs/current/tutorial.html#tut_variables
- Mais dicas e truques de uso do Fish Shell: https://fishshell.com/docs/current/tutorial.html#moretutorials