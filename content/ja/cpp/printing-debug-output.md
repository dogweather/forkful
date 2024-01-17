---
title:                "デバッグ出力の印刷"
html_title:           "C++: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何かしら? 

デバッグ出力のプリントとは、プログラマーがコードの実行中に何が起きているかを確認するために、プログラムの中にメッセージを手動で追加することです。プログラマーがコードを実行する際に、何が起きているかを把握するためにデバッグ出力を使用します。

## 使い方:

```C++
#include <iostream>
using namespace std;

int main() {
  // debug outputを単純に追加する方法
  cout << "デバッグ出力を追加しました。" << endl;

  int num = 5;
  // 変数の値もデバッグ出力に含めることができます
  cout << "numの値は" << num << "です。" << endl;

  // 条件文の中でデバッグ出力を行う例
  if (num > 10) {
    cout << "numは10より大きいです。" << endl;
  } else {
    cout << "numは10以下です。" << endl;
  }

  return 0;
}
```

Output:

```bash
デバッグ出力を追加しました。
numの値は5です。
numは10以下です。
```

## 深層掘り進める: 

デバッグ出力は、プログラミングの歴史とともに発展してきました。過去には、プログラマーがコードに特別な記号を追加することでデバッグ出力を行っていましたが、現代では多くの言語が独自のデバッガーを提供しています。デバッグ出力は、デバッガーを使用できない状況では依然として有用です。代替手段としては、ログファイルの生成やデバッグ用のライブラリを使用することもあります。実装の詳細としては、多くのプログラムにはデバッグ用の出力を無効にするオプションがあり、これを使用することで実際の動作を遅くすることなくデバッグ出力を含めることができます。

## さらに詳しく:

- [デバッグ(プログラミング) - Wikipedia](https://ja.wikipedia.org/wiki/%E3%83%87%E3%83%90%E3%83%83%E3%82%B0_(%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0))
- [C++ デバッグ出力を追加する方法 (Qiita)](https://qiita.com/usounami/items/6a1a2fce69085a73f34c)