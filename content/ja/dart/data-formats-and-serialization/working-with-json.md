---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:27.529666-07:00
description: "JSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-11T00:14:15.328911-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF08JavaScript Object\u2026"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

JSON（JavaScript Object Notation）を利用する作業は、JSONデータを文字列からDartオブジェクトに解析したり、その逆をしたりすることを含みます。これは、Webやアプリ開発でのデータ交換のための共通のタスクです。プログラマーは、APIからのデータ、設定、またはアプリ内のコンポーネント間通信などを効率的に扱うためにこれを行います。

## 方法:

Dartは`dart:convert`ライブラリを使用してJSONをサポートしており、JSONのエンコードとデコードを直感的に行えるようにしています。以下は基本操作の例を示しています：

**JSON文字列をDartオブジェクトに解析する：**
```dart
import 'dart:convert';

void main() {
  // 例 JSON文字列
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // JSONをDart Mapにデコード
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Hello, ${user['name']}! You are ${user['age']} years old.');
  // 出力: Hello, John! You are 30 years old.
}
```

**DartオブジェクトをJSON文字列にエンコードする：**
```dart
import 'dart:convert';

void main() {
  // 例 Dartオブジェクト
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // Dart MapをJSONにエンコード
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // 出力: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**複雑なモデルに`json_serializable`を使用する：**
複雑なデータモデルに対して、手動でのシリアライズは手間がかかります。`json_serializable`パッケージはこのプロセスを自動化します。追加の設定が必要で、`pubspec.yaml`への依存関係の追加やビルドファイルの作成を含みます。セットアップ後、以下のように使用できます：

1. アノテーションを使用してモデルを定義する：
```dart
import 'package:json_annotation/json_annotation.dart';

part 'user.g.dart';

@JsonSerializable()
class User {
  String name;
  int age;
  String email;
  
  User({required this.name, required this.age, required this.email});
  
  factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);
  Map<String, dynamic> toJson() => _$UserToJson(this);
}
```

2. シリアライズのひな形を生成する：
ビルドランナーコマンドを使用して`user.g.dart`ファイルを生成します：
```shell
flutter pub run build_runner build
```

3. モデルを使用する：
```dart
void main() {
  // JSONをUserに解析
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('User: ${user.name}, Age: ${user.age}');
  // 出力: User: John, Age: 30

  // UserをJSONに戻す
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // 出力: {"name":"John","age":30,"email":"john@example.com"}
}
```

これらの例は、Dartでの基本的および高度なJSONのやり取りを示しており、開発者がアプリケーションのデータシリアライゼーションタスクをスムーズに扱えるようにしています。
