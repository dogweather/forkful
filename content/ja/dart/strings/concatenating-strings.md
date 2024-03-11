---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:45.086184-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u6587\
  \u5B57\u5217\u306E\u7D50\u5408\u3068\u306F\u30012\u3064\u4EE5\u4E0A\u306E\u6587\u5B57\
  \u5217\u30921\u3064\u306B\u307E\u3068\u3081\u308B\u3053\u3068\u3092\u6307\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\
  \u3053\u3068\u3067\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u3092\u7C21\u5358\u306B\
  \u64CD\u4F5C\u3057\u305F\u308A\u3001\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u69CB\u7BC9\
  \u3057\u305F\u308A\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\
  \u30FC\u30B9\u306E\u90E8\u54C1\u3092\u52D5\u7684\u306B\u7D44\u307F\u7ACB\u3066\u305F\
  \u308A\u3057\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.290871-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u6587\
  \u5B57\u5217\u306E\u7D50\u5408\u3068\u306F\u30012\u3064\u4EE5\u4E0A\u306E\u6587\u5B57\
  \u5217\u30921\u3064\u306B\u307E\u3068\u3081\u308B\u3053\u3068\u3092\u6307\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\
  \u3053\u3068\u3067\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u3092\u7C21\u5358\u306B\
  \u64CD\u4F5C\u3057\u305F\u308A\u3001\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u69CB\u7BC9\
  \u3057\u305F\u308A\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\
  \u30FC\u30B9\u306E\u90E8\u54C1\u3092\u52D5\u7684\u306B\u7D44\u307F\u7ACB\u3066\u305F\
  \u308A\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u7D50\u5408"
---

{{< edit_this_page >}}

## はじめに
プログラミングにおける文字列の結合とは、2つ以上の文字列を1つにまとめることを指します。プログラマーはこれを行うことでテキストデータを簡単に操作したり、メッセージを構築したり、ユーザーインターフェースの部品を動的に組み立てたりします。

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
