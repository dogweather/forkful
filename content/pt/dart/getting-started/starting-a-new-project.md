---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:35.385289-07:00
description: "Iniciar um novo projeto em Dart envolve a configura\xE7\xE3o de um ambiente\
  \ prop\xEDcio ao desenvolvimento, teste e implementa\xE7\xE3o eficientes. Programadores\
  \ iniciam\u2026"
lastmod: '2024-03-09T21:06:10.629636-07:00'
model: gpt-4-0125-preview
summary: "Iniciar um novo projeto em Dart envolve a configura\xE7\xE3o de um ambiente\
  \ prop\xEDcio ao desenvolvimento, teste e implementa\xE7\xE3o eficientes. Programadores\
  \ iniciam\u2026"
title: Iniciando um novo projeto
---

{{< edit_this_page >}}

## O Que e Por Quê?

Iniciar um novo projeto em Dart envolve a configuração de um ambiente propício ao desenvolvimento, teste e implementação eficientes. Programadores iniciam novos projetos em Dart para aproveitar o desempenho ótimo e o ecossistema robusto do Dart, especialmente para o desenvolvimento de aplicativos web e móveis com frameworks como o Flutter.

## Como fazer:

1. **Instale o Dart**:
   Certifique-se de que o Dart está instalado no seu sistema. Se não, você pode baixá-lo de [https://dart.dev/get-dart](https://dart.dev/get-dart). Verifique a instalação com:

   ```shell
   dart --version
   ```

2. **Crie um Novo Projeto Dart**:
   Use a CLI do Dart para gerar um novo projeto:

   ```shell
   dart create hello_dart
   ```

   Este comando cria um novo diretório `hello_dart` com um simples exemplo de aplicação web ou de console, dependendo da sua seleção.

3. **Examine a Estrutura do Projeto**:
   
   Navegue até o diretório do seu projeto:

   ```shell
   cd hello_dart
   ```

   Um projeto Dart típico inclui os seguintes arquivos e diretórios chave:

   - `pubspec.yaml`: Arquivo de configuração que inclui as dependências do seu projeto e as restrições do SDK.
   - `lib/`: Diretório onde reside a maior parte do código Dart.
   - `test/`: Diretório para os testes do projeto.

4. **Adicione Dependências**:
   Edite `pubspec.yaml` para adicionar dependências. Para projetos web, considere adicionar `http`, um pacote popular para fazer solicitações HTTP:

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   Após a edição, obtenha as dependências:

   ```shell
   dart pub get
   ```

5. **Escreva Seu Primeiro Código Dart**:
   
   No diretório `lib/`, crie um novo arquivo Dart, `main.dart`, e adicione um código Dart simples:

   ```dart
   // Importa a biblioteca central do Dart
   import 'dart:core';

   void main() {
     print('Hello, Dart!');
   }
   ```

6. **Execute Sua Aplicação Dart**:

   Execute seu programa Dart com:

   ```shell
   dart run
   ```

   A saída deve ser:

   ```
   Hello, Dart!
   ```

Seguindo estes passos, você iniciou com sucesso um novo projeto Dart, desde a instalação até a execução do seu primeiro código em Dart. Este conhecimento fundamental prepara o cenário para mergulhar mais fundo no rico ecossistema do Dart e em suas capacidades para construir aplicações escaláveis.
