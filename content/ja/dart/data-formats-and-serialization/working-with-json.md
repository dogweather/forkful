---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:27.529666-07:00
description: "JSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:41.728485-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF08JavaScript Object Notation\uFF09\u3092\u5229\u7528\u3059\u308B\
  \u4F5C\u696D\u306F\u3001JSON\u30C7\u30FC\u30BF\u3092\u6587\u5B57\u5217\u304B\u3089\
  Dart\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u89E3\u6790\u3057\u305F\u308A\u3001\
  \u305D\u306E\u9006\u3092\u3057\u305F\u308A\u3059\u308B\u3053\u3068\u3092\u542B\u307F\
  \u307E\u3059\u3002\u3053\u308C\u306F\u3001Web\u3084\u30A2\u30D7\u30EA\u958B\u767A\
  \u3067\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\u305F\u3081\u306E\u5171\u901A\u306E\
  \u30BF\u30B9\u30AF\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  API\u304B\u3089\u306E\u30C7\u30FC\u30BF\u3001\u8A2D\u5B9A\u3001\u307E\u305F\u306F\
  \u30A2\u30D7\u30EA\u5185\u306E\u30B3\u30F3\u30DD\u30FC\u30CD\u30F3\u30C8\u9593\u901A\
  \u4FE1\u306A\u3069\u3092\u52B9\u7387\u7684\u306B\u6271\u3046\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

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
