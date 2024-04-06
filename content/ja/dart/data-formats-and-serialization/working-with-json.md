---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:27.529666-07:00
description: "\u65B9\u6CD5: Dart\u306F`dart:convert`\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3092\u4F7F\u7528\u3057\u3066JSON\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u304A\
  \u308A\u3001JSON\u306E\u30A8\u30F3\u30B3\u30FC\u30C9\u3068\u30C7\u30B3\u30FC\u30C9\
  \u3092\u76F4\u611F\u7684\u306B\u884C\u3048\u308B\u3088\u3046\u306B\u3057\u3066\u3044\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306F\u57FA\u672C\u64CD\u4F5C\u306E\u4F8B\u3092\u793A\
  \u3057\u3066\u3044\u307E\u3059\uFF1A **JSON\u6587\u5B57\u5217\u3092Dart\u30AA\u30D6\
  \u30B8\u30A7\u30AF\u30C8\u306B\u89E3\u6790\u3059\u308B\uFF1A**."
lastmod: '2024-04-05T21:53:42.646384-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306F`dart:convert`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\
  \u3057\u3066JSON\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u304A\u308A\u3001JSON\u306E\
  \u30A8\u30F3\u30B3\u30FC\u30C9\u3068\u30C7\u30B3\u30FC\u30C9\u3092\u76F4\u611F\u7684\
  \u306B\u884C\u3048\u308B\u3088\u3046\u306B\u3057\u3066\u3044\u307E\u3059\u3002\u4EE5\
  \u4E0B\u306F\u57FA\u672C\u64CD\u4F5C\u306E\u4F8B\u3092\u793A\u3057\u3066\u3044\u307E\
  \u3059\uFF1A **JSON\u6587\u5B57\u5217\u3092Dart\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \u306B\u89E3\u6790\u3059\u308B\uFF1A**."
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
