---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:20.128856-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.709190-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u307E\u3068\u3081\u308B\
  \u3068\u306F\u3001\u5177\u4F53\u7684\u306A\u30BF\u30B9\u30AF\u3092\u5B9F\u884C\u3059\
  \u308B\u518D\u5229\u7528\u53EF\u80FD\u306A\u30B3\u30FC\u30C9\u30D6\u30ED\u30C3\u30AF\
  \u3092\u5B9A\u7FA9\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306F\u901A\
  \u5E38\u3001\u5165\u529B\u3092\u53D7\u3051\u53D6\u308A\u3001\u30C7\u30FC\u30BF\u3092\
  \u51E6\u7406\u3057\u3001\u5834\u5408\u306B\u3088\u3063\u3066\u306F\u51FA\u529B\u3092\
  \u8FD4\u3059\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u306E\u53EF\u8AAD\u6027\u3092\u9AD8\
  \u3081\u3001\u91CD\u8907\u3092\u6E1B\u3089\u3057\u3001\u4FDD\u5B88\u3092\u5BB9\u6613\
  \u306B\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002\
  \u7D50\u679C\u3068\u3057\u3066\u3001\u3088\u308A\u30E2\u30B8\u30E5\u30FC\u30EB\u5F0F\
  \u3067\u7BA1\u7406\u3057\u3084\u3059\u3044\u30B3\u30FC\u30C9\u30D9\u30FC\u30B9\u306B\
  \u3064\u306A\u304C\u308A\u307E\u3059\u3002."
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

## 方法:


### 基本的な関数
Dartでは、関数は`void`キーワードを使用して定義します。これは、値を返さない場合に使用します。また、それ以外の場合は返される値のタイプを指定します。以下は、挨拶メッセージを印刷するシンプルな関数の例です:

```dart
void greet(String name) {
  print('Hello, $name!');
}

void main() {
  greet('Alice');  // 出力: Hello, Alice!
}
```

### 値を返す
関数は値を返すことができます。次の例は、二つの整数を入力として受け取り、それらの合計を返します:

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var sum = add(5, 3);
  print(sum);  // 出力: 8
}
```

### 匿名関数
Dartは、その場での短い機能に便利な匿名関数（ラムダ式またはクロージャとしても知られている）をサポートします。リストの`forEach`メソッドで匿名関数を使用する方法は以下の通りです:

```dart
void main() {
  var fruits = ['apple', 'banana', 'cherry'];
  fruits.forEach((item) {
    print(item);
  });
  // 出力:
  // apple
  // banana
  // cherry
}
```

### シングルエクスプレッション関数のためのアロー構文
一つの式のみを含む関数については、Dartは「アロー」記法（`=>`）を使用した簡潔な構文を提供します。これは、特に短い関数や関数を引数として渡す場合に便利です:

```dart
int square(int num) => num * num;

void main() {
  print(square(4));  // 出力: 16
}
```

### サードパーティのライブラリを使用する
より複雑または特化した機能については、Dartプログラマーはしばしばサードパーティのライブラリに頼ります。HTTPリクエストを行うための`http`ライブラリを考えてみましょう。まず、依存関係の下に`http`をあなたのpubspec.yamlファイルに追加します:

```
dependencies:
  http: ^0.13.3
```

それから、ウェブからデータをフェッチするためにそれを使用できます:

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var response = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(response.body);
}

void main() {
  fetchUserData();
  // 期待される出力: ユーザーのJSONデータ。実際の出力はAPIの応答によって異なります。
}
```

Dartコードを関数にまとめるときは、再利用性、明確性、単一責任の原則について考えてください。これにより、コードはよりクリーンになり、他の人（そして未来のあなた）が理解して保守しやすくなります。
