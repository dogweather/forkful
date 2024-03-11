---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:23.204047-07:00
description: "Iniciar um novo projeto em Go envolve configurar um espa\xE7o de trabalho\
  \ e inicializ\xE1-lo com os m\xF3dulos Go necess\xE1rios. Os programadores fazem\
  \ isso para\u2026"
lastmod: '2024-03-11T00:14:19.721772-06:00'
model: gpt-4-0125-preview
summary: "Iniciar um novo projeto em Go envolve configurar um espa\xE7o de trabalho\
  \ e inicializ\xE1-lo com os m\xF3dulos Go necess\xE1rios. Os programadores fazem\
  \ isso para\u2026"
title: Iniciando um novo projeto
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Iniciar um novo projeto em Go envolve configurar um espaço de trabalho e inicializá-lo com os módulos Go necessários. Os programadores fazem isso para organizar o código, gerenciar dependências de forma eficaz e facilitar os processos de construção. É fundamental para criar software escalável e sustentável em Go.

## Como fazer:

Primeiro, certifique-se de que tem o Go instalado, executando `go version` no seu terminal. Você deverá ver a versão do Go que instalou como saída. A seguir, vamos começar um novo projeto. Navegue até o seu espaço de trabalho e execute:

```shell
mkdir hello-world
cd hello-world
```

Isso cria e move você para um novo diretório do seu projeto. Agora, inicialize o módulo:

```shell
go mod init exemplo.com/ola-mundo
```

Substitua `exemplo.com/ola-mundo` pelo caminho do seu módulo. Este comando cria um arquivo `go.mod` no seu diretório, sinalizando o início de um novo módulo Go. Eis como pode parecer o `go.mod`:

```plaintext
module exemplo.com/ola-mundo

go 1.18
```

`go.mod` rastreia as dependências do seu projeto. Agora, crie um arquivo `main.go`:

```shell
touch main.go
```

Abra `main.go` no seu editor preferido e adicione o seguinte código para imprimir "Olá, Mundo!":

```go
package main

import "fmt"

func main() {
    fmt.Println("Olá, Mundo!")
}
```

Para executar o seu programa, volte ao terminal e execute:

```shell
go run main.go
```

Você deverá ver:

```plaintext
Olá, Mundo!
```

Parabéns! Você acaba de iniciar um novo projeto Go e rodou seu primeiro programa Go.

## Aprofundando

A iniciativa de introduzir módulos como o padrão para gerenciamento de dependências no Go foi uma mudança significativa no ecossistema Go, oficialmente adotada no Go 1.11. Antes dos módulos, os desenvolvedores Go confiavam na variável de ambiente GOPATH para gerenciar dependências, o que era menos intuitivo e muitas vezes levava ao infame "inferno das dependências".

Módulos fornecem uma forma encapsulada de gerenciar dependências do projeto, versionamento e são um passo em direção a tornar os projetos Go mais autocontidos e portáteis. Cada módulo especifica suas dependências que o Go rastreia no arquivo `go.mod`, simplificando o gerenciamento de dependências em diferentes ambientes e etapas de desenvolvimento.

No entanto, vale ressaltar que, embora os módulos Go sejam agora o padrão, alguns projetos legados ainda podem usar o GOPATH. Para a maioria dos novos projetos, os módulos oferecem um sistema de gerenciamento mais simples e eficaz, mas entender o GOPATH pode ser útil para manter ou contribuir para bases de código Go mais antigas.

Em termos de alternativas, enquanto os módulos Go são agora o padrão de facto, a comunidade Go experimentou com outras ferramentas de gerenciamento de dependências como o `dep` no passado. No entanto, esses foram amplamente superados pelo suporte oficial de módulos integrado na cadeia de ferramentas Go.
