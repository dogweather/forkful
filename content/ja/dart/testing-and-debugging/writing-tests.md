---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:03.607943-07:00
description: "Dart\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\
  \u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u7570\u306A\u308B\u90E8\u5206\u304C\u671F\u5F85\
  \u901A\u308A\u306B\u6A5F\u80FD\u3059\u308B\u304B\u3092\u81EA\u52D5\u7684\u306B\u691C\
  \u8A3C\u3059\u308B\u30C6\u30B9\u30C8\u30B1\u30FC\u30B9\u3092\u4F5C\u6210\u3059\u308B\
  \u3053\u3068\u3092\u542B\u3093\u3067\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30B3\u30FC\
  \u30C9\u304C\u4FE1\u983C\u6027\u304C\u9AD8\u304F\u3001\u6B20\u9665\u304C\u306A\u3044\
  \u3053\u3068\u3092\u78BA\u8A8D\u3057\u3001\u66F4\u65B0\u3084\u30EA\u30D5\u30A1\u30AF\
  \u30BF\u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3057\u306A\u304C\u3089\u3001\u56DE\
  \u5E30\u3092\u9632\u304E\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.308462-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\u30D7\
  \u30ED\u30B0\u30E9\u30E0\u306E\u7570\u306A\u308B\u90E8\u5206\u304C\u671F\u5F85\u901A\
  \u308A\u306B\u6A5F\u80FD\u3059\u308B\u304B\u3092\u81EA\u52D5\u7684\u306B\u691C\u8A3C\
  \u3059\u308B\u30C6\u30B9\u30C8\u30B1\u30FC\u30B9\u3092\u4F5C\u6210\u3059\u308B\u3053\
  \u3068\u3092\u542B\u3093\u3067\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30B3\u30FC\u30C9\
  \u304C\u4FE1\u983C\u6027\u304C\u9AD8\u304F\u3001\u6B20\u9665\u304C\u306A\u3044\u3053\
  \u3068\u3092\u78BA\u8A8D\u3057\u3001\u66F4\u65B0\u3084\u30EA\u30D5\u30A1\u30AF\u30BF\
  \u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3057\u306A\u304C\u3089\u3001\u56DE\u5E30\
  \u3092\u9632\u304E\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？

Dartでテストを書くことは、プログラムの異なる部分が期待通りに機能するかを自動的に検証するテストケースを作成することを含んでいます。プログラマーはこれを行うことで、コードが信頼性が高く、欠陥がないことを確認し、更新やリファクタリングを容易にしながら、回帰を防ぎます。

## どうやって：

Dartでは、`test`パッケージがテストの記述に一般的に使用されます。まず、`pubspec.yaml`に`test`パッケージを追加します：

```yaml
dev_dependencies:
  test: ^1.0.0
```

次に、シンプルな関数のテストを書きます。2つの数値を加算する関数があるとします：

```dart
int add(int a, int b) {
  return a + b;
}
```

次に、`test`ディレクトリ内に`add_test.dart`という名前のファイルを作成し、テストケースを記述します：

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // `add`関数が lib/add.dart 内にあると仮定

void main() {
  test('2つの数値の加算', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

テストを実行するには、Dartコマンドを使用します：

```bash
$ dart test
```

サンプル出力は次のようになるかもしれません：

```
00:01 +1: 全てのテストが通過しました！
```

### サードパーティライブラリを使用する：モッキングのためのMockito 

複雑な依存関係を持つコードをテストする場合、モックオブジェクトを作成するためにMockitoを使用するかもしれません。まず、`pubspec.yaml`にMockitoを追加します：

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

ユーザーデータを取得する`UserRepository`クラスがあり、実際のデータベースにアクセスせずに`UserRepository`に依存する`UserService`をテストしたいとします：

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Mockitoを使用してMockクラスを作成
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('UserService テスト', () {
    test('ユーザーを正常に取得', () {
      // モックインスタンスの作成
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // モックの振る舞いのセットアップ
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'テストユーザー'));

      // モックメソッドが期待される引数で呼び出されたことをアサート
      expect(userService.getUserName(1), 'テストユーザー');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

このテストの実行は、`UserService`が`UserRepository`と正しく相互作用し、モッキングを使用して実際の相互作用を制御された方法でシミュレートすることを確認します。
