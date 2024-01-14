---
title:                "Swift: Iniciando um novo projeto"
programming_language: "Swift"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto em Swift?

Começar um novo projeto em Swift pode trazer uma série de benefícios, como aprender uma nova linguagem de programação, criar aplicativos para dispositivos móveis e desenvolver habilidades em programação orientada a objetos.

## Como fazer:

Para começar um novo projeto em Swift, é necessário ter acesso a um computador com Xcode instalado. Xcode é o IDE (Integrated Development Environment) da Apple para desenvolvimento em Swift e permite criar aplicativos para iOS, macOS e outras plataformas Apple.

O primeiro passo é criar um novo projeto no Xcode. Você pode fazer isso selecionando a opção “Criar um novo projeto” no menu File ou pressionando Command + Shift + N. Na janela que abrir, selecione “Single View App” como tipo de projeto e clique em Next.

Em “Product Name”, dê um nome ao seu projeto. Certifique-se de selecionar “Swift” como linguagem de programação e “Storyboard” como interface de usuário. Você pode deixar as outras opções como padrão e clicar em Next. Escolha um local para salvar o projeto e clique em Create.

Agora, você pode começar a escrever seu código Swift. Abra o arquivo ViewController.swift e você verá o seguinte código:

```Swift
import UIKit

class ViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()
        // Write your code here
    }
}
```

Dentro da função `viewDidLoad()`, você pode começar a escrever seu código para criar seu aplicativo. Por exemplo, para exibir uma mensagem na tela, você pode adicionar o seguinte código:

```Swift
print("Olá Swift!")
```

Para verificar a saída, você pode pressionar o botão “Play” no canto superior esquerdo da tela e selecionar “Run” no menu suspenso. Você também pode usar o atalho Command + R. Isso irá executar seu aplicativo em um simulador ou dispositivo real, se estiver conectado.

## Aprofundando:

Ao começar um novo projeto em Swift, é importante entender os conceitos fundamentais da linguagem, como tipos de dados, estruturas de controle, funções e classes. Também é importante familiarizar-se com o desenvolvimento para dispositivos móveis e as melhores práticas de design de aplicativos.

Uma ótima maneira de se aprofundar em Swift é através de tutoriais e cursos online, como o Swift Playgrounds da Apple e o Curso de Swift do Udacity. Também é útil participar de meetups e conferências para se conectar com outros desenvolvedores e trocar experiências e conhecimentos.

## Veja também:

- [Swift Playgrounds](https://www.apple.com/swift/playgrounds/)
- [Curso de Swift do Udacity](https://www.udacity.com/course/swift-for-beginners--ud1022)
- [Meetup de desenvolvedores iOS no Brasil](https://www.meetup.com/pt-BR/topics/ios-development/br/)
- [Conferência WWDC da Apple](https://developer.apple.com/wwdc/)