---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:45.086184-07:00
description: "\u65B9\u6CD5 Dart\u306B\u306F\u6587\u5B57\u5217\u3092\u7D50\u5408\u3059\
  \u308B\u305F\u3081\u306E\u3044\u304F\u3064\u304B\u306E\u7C21\u5358\u306A\u65B9\u6CD5\
  \u304C\u3042\u308A\u307E\u3059\u3002\u4EE5\u4E0B\u304C\u6700\u3082\u4E00\u822C\u7684\
  \u306A\u65B9\u6CD5\u3067\u3059\uFF1A #."
lastmod: '2024-03-13T22:44:41.685262-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306B\u306F\u6587\u5B57\u5217\u3092\u7D50\u5408\u3059\u308B\u305F\u3081\
  \u306E\u3044\u304F\u3064\u304B\u306E\u7C21\u5358\u306A\u65B9\u6CD5\u304C\u3042\u308A\
  \u307E\u3059\u3002\u4EE5\u4E0B\u304C\u6700\u3082\u4E00\u822C\u7684\u306A\u65B9\u6CD5\
  \u3067\u3059\uFF1A\n\n#."
title: "\u6587\u5B57\u5217\u306E\u7D50\u5408"
weight: 3
---

## 方法
Dartには文字列を結合するためのいくつかの簡単な方法があります。以下が最も一般的な方法です：

### `+` 操作子を使用
`+` 操作子は文字列を結合する最も直感的な方法です。
```dart
String greeting = 'Hello, ' + 'World!';
print(greeting); // 出力：Hello, World!
```

### `concat()` メソッドの使用
Dartには他の言語にあるような`concat()`メソッドはありませんが、`+`や以下の方法を使用して同じことが達成できます。

### 文字列補間の使用
文字列補間では、変数を直接文字列に埋め込むことができます。これは文字列や式を組み合わせるのに効率的です。
```dart
String user = 'Jane';
String message = 'Welcome, $user!';
print(message); // 出力：Welcome, Jane!
```

### `join()` メソッドの使用
`join()` メソッドは、連結したい文字列のリストがあるときに便利です。
```dart
var words = ['Hello', 'from', 'Dart'];
String sentence = words.join(' '); // スペース区切りで結合。
print(sentence); // 出力：Hello from Dart
```

### StringBufferの使用
`StringBuffer` は、特にループ内での複数の結合に効率的です。
```dart
var words = ['Dart', 'is', 'fun'];
StringBuffer buffer = StringBuffer();
for (String word in words) {
  buffer.write(word); // 各単語をバッファに追加。
  buffer.write(' '); // オプションでスペースを追加。
}
String sentence = buffer.toString().trim(); // 文字列に変換し、末尾の空白を削除。
print(sentence); // 出力：Dart is fun
```

### サードパーティライブラリ
Dartの標準ライブラリだけで文字列の結合タスクには通常十分ですが、`quiver`のようなサードパーティライブラリは、Dartの組み込み機能を補完するユーティリティを提供する場合があります。たとえば、`quiver`の`concat()`や`merge()`関数は、高度なシナリオで探求されるかもしれません。しかし、必要な特定のニーズを標準オプションがカバーしていない場合を除き、Dartの堅牢な組み込みオプションに固執しましょう。
