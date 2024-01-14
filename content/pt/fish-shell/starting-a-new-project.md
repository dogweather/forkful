---
title:                "Fish Shell: Começando um novo projeto"
simple_title:         "Começando um novo projeto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que usar a Fish Shell para começar um novo projeto?

Se você é um programador que adora trabalhar com linhas de comando e busca por uma shell mais intuitiva, completa e fácil de usar, a resposta é simples: a Fish Shell é a escolha perfeita para você. Com recursos como auto-completar com sugestões, uma linguagem de scripting simples e poderosa e uma comunidade ativa, a Fish Shell é a ferramenta ideal para iniciar um novo projeto e aumentar sua produtividade.

## Como usar a Fish Shell para iniciar um novo projeto

Para utilizar a Fish Shell, você precisa primeiro instalá-la no seu sistema. Dependendo do seu sistema operacional, isso pode ser feito facilmente através do gerenciador de pacotes, como o apt-get no Ubuntu ou o Homebrew no Mac. Depois de instalada, basta abrir o terminal e digitar "fish" para iniciar a sua nova shell.

Uma das principais características da Fish Shell é o recurso de auto-completar com sugestões. Isso significa que a shell irá sugerir possíveis comandos e argumentos à medida que você digita, economizando seu tempo e prevenindo erros. Por exemplo, se você digitar "cd D" e pressionar a tecla Tab, a shell irá sugerir automaticamente o diretório "Documents" caso ele exista no seu sistema.

Outra funcionalidade importante é a linguagem de scripting da Fish Shell, que é baseada em comandos e funções simples. Por exemplo, para criar uma função que liste os arquivos de uma determinada pasta, basta digitar o seguinte código no terminal:

```Fish Shell

function listar
ls $argv
end
```

Ao chamar a função "listar" com o nome da pasta como argumento, a shell irá executar o comando "ls" e listar todos os arquivos daquela pasta. Além disso, a Fish Shell possui uma extensa documentação e uma comunidade ativa, o que facilita o aprendizado e a resolução de possíveis problemas.

## Mais informações sobre iniciar um novo projeto com a Fish Shell

Além dos recursos já mencionados, a Fish Shell também oferece outras funcionalidades que podem tornar o processo de iniciar um novo projeto mais eficiente. Uma delas é a capacidade de criar e gerenciar aliases, que são comandos personalizados que podem substituir sequências mais longas de comandos.

Além disso, a Fish Shell também suporta a utilização de plugins, que são pequenos programas desenvolvidos pela comunidade para adicionar novas funcionalidades à shell. Com isso, é possível personalizar ainda mais a sua experiência de uso.

## Veja também

- [Documentação oficial da Fish Shell] (https://fishshell.com/docs/current/)
- [Repositório do projeto no GitHub] (https://github.com/fish-shell/fish-shell)
- [Comunidade de usuários da Fish Shell] (https://fishshell.com/community.html)

Agora que você conhece um pouco mais sobre a Fish Shell, não perca mais tempo e comece a utilizá-la em seus projetos. Sua produtividade e facilidade na linha de comando certamente irão aumentar. Até a próxima!