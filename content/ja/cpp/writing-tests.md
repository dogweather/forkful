---
title:                "テストの書き方"
html_title:           "C++: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## 何&なぜ？

テストを書くこととは、プログラマーがコードの正しさを確認するために行う作業です。テストを書くことで、プログラムのバグやエラーを発見し、品質の高いソフトウェアを作ることができます。

## 方法：

```C++
// テストを書く基本的な構文
#include <iostream>

int main() {

  // テストを書くための関数
  void testFunc();

  // テストの実行
  testFunc();

  return 0;
}

// テストを書く関数の定義
void testFunc() {

  // テストしたいコードを記述する
  int num1 = 5;
  int num2 = 10;
  int result = num1 + num2;

  // 結果の出力
  std::cout << result << std::endl;
}

```

実行結果：
15

## 深く掘り下げる：

テストの歴史的な背景については、最初のプログラミング言語であるFORTRANからその重要性が認識されてきました。テストの代替手段として、デバッグやデプロイメントの際の手動テストやユニットテストがあります。また、テストの実装方法には、手書きのテストコードや自動化されたテストプログラムの使用があります。

## 関連リンク：

- [Google テスト](https://github.com/google/googletest)