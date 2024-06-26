---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:35.385289-07:00
description: "Como fazer: 1. **Instale o Dart**: Certifique-se de que o Dart est\xE1\
  \ instalado no seu sistema. Se n\xE3o, voc\xEA pode baix\xE1-lo de [https://dart.dev/get-\u2026"
lastmod: '2024-04-05T22:38:45.427184-06:00'
model: gpt-4-0125-preview
summary: "1. **Instale o Dart**: Certifique-se de que o Dart est\xE1 instalado no\
  \ seu sistema. Se n\xE3o, voc\xEA pode baix\xE1-lo de [https://dart.dev/get-dart](https://dart.dev/get-dart).\
  \ Verifique a instala\xE7\xE3o com."
title: Iniciando um novo projeto
weight: 1
---

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
