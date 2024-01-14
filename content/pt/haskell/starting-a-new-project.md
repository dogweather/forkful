---
title:                "Haskell: Iniciando um novo projeto"
programming_language: "Haskell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto em Haskell?

Haskell é uma linguagem de programação funcional pura que oferece diversos benefícios, como segurança de tipos de dados, expressividade e modularidade. Começar um novo projeto em Haskell pode ser uma ótima escolha para aqueles que desejam desenvolver softwares confiáveis e escaláveis.

## Como fazer isso?

Para iniciar um novo projeto em Haskell, você pode seguir os seguintes passos:

1. Instale o compilador GHC e o gerenciador de pacotes Cabal em sua máquina.
2. Crie um novo diretório para o seu projeto e navegue até ele pelo terminal.
3. Inicie um novo projeto Haskell com o comando `cabal init`.
4. Escolha um nome para o seu pacote, preencha as informações solicitadas e selecione as dependências necessárias.
5. Comece a codificar! Você pode usar o editor de texto de sua preferência ou até mesmo IDEs especializadas em Haskell, como o Visual Studio Code com a extensão "Haskell Language Features".

Aqui está um exemplo simples de código Haskell no qual é solicitado um número ao usuário e, em seguida, é exibida a sua raiz quadrada:

```haskell
import Text.Read

main = do
    putStrLn "Digite um número:"
    input <- getLine
    let number = readMaybe input :: Maybe Double
    case number of
        Just n -> putStrLn ("A raiz quadrada de " ++ show n ++ " é " ++ show (sqrt n))
        Nothing -> putStrLn "Número inválido!"
```

O programa acima solicita um número ao usuário através do terminal e, em seguida, converte essa entrada para um número do tipo `Double`. Se a conversão for bem-sucedida, o programa usa a função `sqrt` para calcular a raiz quadrada do número e exibe o resultado na tela. Caso contrário, uma mensagem de erro é exibida.

## Mergulho profundo

Ao iniciar um novo projeto em Haskell, é importante ter em mente algumas boas práticas para garantir um desenvolvimento mais suave e eficiente. Aqui estão algumas dicas úteis:

- Planeje seu código antes de começar a codificar. Haskell é uma linguagem muito expressiva, mas é sempre melhor ter uma visão geral do problema e das soluções antes de escrever o código.
- Use tipos de dados personalizados para representar suas entidades e dados. Isso ajudará a tornar seu código mais legível e manutenível.
- Faça uso de funções de ordem superior, como `map`, `filter` e `fold`, para manipular listas de forma concisa e elegante.
- Teste seu código constantemente. Haskell possui uma forte tipagem estática, o que significa que os erros só são detectados em tempo de compilação. Testar seu código ajudará a encontrar possíveis bugs e garantir a qualidade do seu software.

## Veja também

- [Documentação oficial do Haskell](https://www.haskell.org/documentation/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [How to Haskell](https://github.com/bitemyapp/learnhaskell)
- [Haskell for all - Comprehensive Haskell tutorials](https://www.haskellforall.com/)