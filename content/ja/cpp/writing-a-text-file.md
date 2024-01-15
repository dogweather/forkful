---
title:                "テキストファイルの作成"
html_title:           "C++: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを作成することの目的の一つは、大量のデータを保存し、繰り返し使用することができるようにすることです。また、プログラムの実行中に必要な情報を保存するのにも便利です。

## 方法

例として、`textfile.cpp`という名前のC++のプログラムを作成します。まず、`<fstream>`というヘッダーファイルをインクルードします。次に、`ofstream`というクラスを使用して、新しいテキストファイルを作成します。

```
#include <fstream>
using namespace std;

int main() {
  ofstream file("example.txt"); // 新しいテキストファイルの作成
}
```

次に、作成したファイルにテキストを書き込む方法を見ていきましょう。まず、`<<`演算子を使用して、文字列を追加します。その後、`close()`メソッドを使用してファイルを閉じます。

```
#include <fstream>
using namespace std;

int main() {
  ofstream file("text.txt"); // 新しいテキストファイルの作成
  file << "Hello World!" << endl; // ファイルに文字列を追加
  file.close(); // ファイルを閉じる
}
```

このプログラムを実行すると、`example.txt`ファイルが作成され、ファイル内に「Hello World!」というテキストが書き込まれます。また、既存のテキストファイルに追加する方法もあります。`ofstream`の代わりに`ofstream::app`を使用することで、ファイル末尾に新しいテキストを追加することができます。

```
#include <fstream>
using namespace std;

int main() {
  ofstream file("text.txt", ofstream::app); // 既存のファイルに追加
  file << "This is a new line." << endl; // 既存のファイルに追加
  file.close(); // ファイルを閉じる
}
```

## 深堀り

テキストファイルを作成するためには、`stream`クラスを使用します。`ostream`は、データを出力するための基本的なクラスであり、`ofstream`はその派生クラスです。ただし、`ofstream`はファイルを作成し、ファイル内にデータを追加することができる点が異なります。

また、`<<`演算子を使用することで、様々なデータ型をファイル内に書き込むことができます。例えば、数値データや文字列、変数の値などを書き込むことができます。

## 関連記事

- [C++ 公式ページ](https://isocpp.org/)
- [C++リファレンス](https://cpprefjp.github.io/)
- [C++のファイル入出力について](https://www.geeksforgeeks.org/working-files-c-cpp/)