---
title:                "デバッグ出力のプリント"
html_title:           "C++: デバッグ出力のプリント"
simple_title:         "デバッグ出力のプリント"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜデバッグ出力を記述するのか？

デバッグ出力は、コードの実行中にプログラマーが特定の変数の値やプログラムの動作を確認するために使用されます。このように、デバッグ出力はプログラムのバグを見つけるために非常に便利です。

## デバッグ出力の方法

デバッグ出力を行うには、C++で`cout`を使用します。以下のように、変数の値やメッセージを画面に出力することができます。

```C++
int num = 10;
cout << "変数の値は: " << num << endl;
```

このコードを実行すると、`変数の値は: 10`というメッセージが表示されます。また、条件分岐やループなどの制御構造の中でもデバッグ出力を使用することができます。例えば、以下のように`if`文の中で変数の値を出力することができます。

```C++
if (num > 0) {
  cout << "numは正の数です" << endl;
} else {
  cout << "numは負の数です" << endl;
}
```

## デバッグ出力の詳細

デバッグ出力を活用する際には、`cout`の他にも`cerr`や`clog`などのストリームを使用することもできます。また、`setw`や`setprecision`などのフォーマット指定子を使うことで、デバッグ出力をより見やすくすることができます。さらに、デバッグ出力をファイルに保存することもでき、後から確認することができます。

## 関連記事

- [C++の入門: 標準出力](https://www.cplusplus.com/reference/iostream/cout/)
- [C++のデバッグテクニック](https://www.codeguru.com/cpp/cpp/cpp_debugging/)
- [デバッグ出力を活用するためのTips](https://www.ibm.com/docs/ja/zosbasic?topic=formats-using-debug-output-tips)

# 関連リンク

- [C++リファレンス: 標準入出力ライブラリ](https://cpprefjp.github.io/reference/cstdio/)
- [C++入門講座](https://programming.pc-note.net/cpp/)