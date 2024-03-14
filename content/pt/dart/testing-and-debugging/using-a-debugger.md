---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:46.143478-07:00
description: "Usar um depurador em Dart permite que os programadores examinem metodicamente\
  \ seu c\xF3digo, configurando pontos de interrup\xE7\xE3o, acompanhando a execu\xE7\
  \xE3o\u2026"
lastmod: '2024-03-13T22:44:46.286557-06:00'
model: gpt-4-0125-preview
summary: "Usar um depurador em Dart permite que os programadores examinem metodicamente\
  \ seu c\xF3digo, configurando pontos de interrup\xE7\xE3o, acompanhando a execu\xE7\
  \xE3o\u2026"
title: Usando um depurador
---

{{< edit_this_page >}}

## O Que e Por Que?

Usar um depurador em Dart permite que os programadores examinem metodicamente seu código, configurando pontos de interrupção, acompanhando a execução passo a passo e inspecionando variáveis. Este processo é essencial para identificar e corrigir bugs de maneira eficiente, tornando-se uma ferramenta indispensável no ciclo de desenvolvimento.

## Como Fazer:

### Depuração Básica:

**1. Configurando Pontos de Interrupção:**

Para configurar um ponto de interrupção, basta clicar na margem esquerda da linha de código no seu IDE (por exemplo, Visual Studio Code ou Android Studio) onde você deseja que a execução seja pausada.

```dart
void main() {
  var message = 'Olá, Depuração';
  print(message); // Configure um ponto de interrupção aqui
}
```

**2. Iniciando a Depuração:**

No seu IDE, inicie uma sessão de depuração clicando no ícone de depuração ou pressionando o botão de depuração. A execução será pausada nos pontos de interrupção.

**3. Inspecionando Variáveis:**

Uma vez que a execução esteja pausada, passe o mouse sobre as variáveis para ver seus valores atuais.

**4. Avançando Pelo Código:**

Use os comandos de avançar, entrar e sair no seu IDE para navegar pelo seu código uma linha ou função por vez.

### Depuração Avançada com Observatory:

Dart inclui uma ferramenta chamada Observatory para depuração e perfilagem de aplicações Dart. É particularmente útil para aplicações que rodam na VM de Dart.

**Acessando o Observatory:**

Execute sua aplicação Dart com o flag `--observe`.

```bash
dart --observe seu_programa.dart
```

Este comando imprime uma URL no console, que você pode abrir em um navegador da web para acessar o depurador do Observatory.

### Usando Bibliotecas de Terceiros Populares:

Para depurar aplicações Flutter, o pacote `flutter_devtools` oferece um conjunto de ferramentas de desempenho e depuração que se integram tanto com a VM de Dart quanto com o Flutter.

**Instalação:**

Primeiro, adicione `devtools` ao seu arquivo `pubspec.yaml` em `dev_dependencies`:

```yaml
dev_dependencies:
  devtools: any
```

**Lançando o DevTools:**

Execute este comando no seu terminal:

```bash
flutter pub global run devtools
```

Em seguida, inicie sua aplicação Flutter em modo de depuração. O DevTools oferece recursos como o inspetor do Flutter para análise da árvore de widgets, e o perfilador de rede para monitoramento da atividade de rede.

### Saída de Amostra:

Ao atingir um ponto de interrupção, seu IDE pode mostrar valores de variáveis e rastreamentos de pilha assim:

```
message: 'Olá, Depuração'
```

Ao aproveitar efetivamente as ferramentas e técnicas de depuração em Dart, os desenvolvedores podem identificar e resolver problemas mais rapidamente, levando a um processo de desenvolvimento mais suave e a aplicações mais robustas.
