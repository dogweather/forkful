---
title:                "文字列の補間"
date:                  2024-03-08T21:55:11.303405-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

文字列補間とは、変数値を直接文字列に注入するプロセスで、面倒な連結を使用せずに意味のあるメッセージを作成することがよくあります。プログラマーは、よりクリーンで読みやすいコードを実現し、複雑な文字列の連結で発生しやすいエラーを防ぐためにこれを行います。

## 方法：

Dartでは、文字列補間は直感的で、`$` 記号を使用して文字列リテラル内に直接式を補間します：

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // 単純な変数の補間
  print('Learning $name in $year!');
  // 出力: Learning Dart in 2023!
  
  // 式の補間
  print('In two years, it will be ${year + 2}.');
  // 出力: In two years, it will be 2025.
}
```

より複雑な式を持っている場合や、文字列自体で操作を行いたい場合は、式を `${}` で囲みます。Dartには文字列補間のための特に人気のあるサードパーティのライブラリはありませんが、さまざまで複雑なシナリオをネイティブにうまく扱えるように十分装備されています。
