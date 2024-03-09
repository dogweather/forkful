---
title:                "テストの作成"
date:                  2024-03-08T21:58:03.607943-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
