---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:03.479015-07:00
description: "Escrever testes em Dart envolve criar casos de teste para verificar\
  \ automaticamente que diferentes partes do seu programa funcionam como esperado.\u2026"
lastmod: '2024-03-13T22:44:46.285543-06:00'
model: gpt-4-0125-preview
summary: "Escrever testes em Dart envolve criar casos de teste para verificar automaticamente\
  \ que diferentes partes do seu programa funcionam como esperado.\u2026"
title: Escrevendo testes
weight: 36
---

## O Que & Por Que?

Escrever testes em Dart envolve criar casos de teste para verificar automaticamente que diferentes partes do seu programa funcionam como esperado. Programadores fazem isso para garantir que seu código seja confiável e livre de defeitos, facilitando atualizações e refatorações mais fáceis, enquanto previne regressões.

## Como fazer:

Em Dart, o pacote `test` é comumente usado para escrever testes. Primeiro, adicione o pacote `test` ao seu `pubspec.yaml`:

```yaml
dev_dependencies:
  test: ^1.0.0
```

Depois, escreva um teste para uma função simples. Suponha que você tenha uma função que adiciona dois números:

```dart
int add(int a, int b) {
  return a + b;
}
```

Em seguida, crie um arquivo chamado `add_test.dart` no diretório `test` e escreva seu caso de teste:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Assuma que sua função `add` está em lib/add.dart

void main() {
  test('adiciona dois números', () {
    var esperado = 3;
    expect(add(1, 2), equals(esperado));
  });
}
```

Para executar os testes, use o comando Dart:

```bash
$ dart test
```

A saída de exemplo pode se parecer com:

```
00:01 +1: Todos os testes passaram!
```

### Usando uma biblioteca de terceiros: Mockito para simulação

Para testar código que possui dependências complexas, você pode usar Mockito para criar objetos simulados. Primeiro, adicione Mockito ao seu `pubspec.yaml`:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Supondo que você tenha uma classe `UserRepository` que busca dados de usuários, e você deseja testar um `UserService` que depende de `UserRepository` sem acessar um banco de dados real:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:seu_projeto/user_repository.dart';
import 'package:seu_projeto/user_service.dart';

// Criar uma classe Mock usando Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('Testes de UserService', () {
    test('Busca usuário com sucesso', () {
      // Criar instância mock
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Configurar comportamento mock
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Usuário Teste'));

      // Afirmar que o método mockado é chamado com os argumentos esperados
      expect(userService.getUserName(1), 'Usuário Teste');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

Executar este teste confirma que o `UserService` interage corretamente com `UserRepository`, usando a simulação para imitar as interações reais de maneira controlada.
