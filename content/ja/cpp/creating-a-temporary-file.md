---
title:                "C++: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

テンポラリファイルを作成することは、プログラマーにとって非常に便利です。プログラムを実行する間に生成された一時的なデータを保存することで、プログラムのメモリ使用量を減らし、パフォーマンスを向上させることができます。

## 作り方

テンポラリファイルを作成するには、<fstream>ヘッダーの```tmpfile()```関数を使用します。以下のコードは、```tmpfile()```関数を用いてテンポラリファイルを作成し、その中に文字列を書き込む例です。

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    // テンポラリファイルを作成
    FILE* temp_file = tmpfile();

    // テンポラリファイルに文字列を書き込む
    fputs("Hello, world!", temp_file);

    // テンポラリファイルを閉じる
    fclose(temp_file);

    return 0;
}
```

このプログラムを実行すると、指定した文字列を含むテンポラリファイルが作成されます。

## もっと掘り下げる

テンポラリファイルは、一時的なデータを保存するだけでなく、プログラムの安全性にも役立ちます。例えば、ファイル入出力処理中にエラーが発生した場合、テンポラリファイルを使用して元のファイルを復元することができます。

また、<cstdio>ヘッダーの```tempnam()```関数を使用することで、ユーザーが場所を指定できる場所にテンポラリファイルを作成することもできます。これにより、プログラムの柔軟性を高めることができます。

## 参考リンク

- [C++ Reference - tmpfile()](https://en.cppreference.com/w/cpp/io/c/tmpfile)
- [C++ Reference - tempnam()](https://en.cppreference.com/w/cpp/io/c/tempnam)
- [C++でテンポラリファイルを作成する方法](https://code-examples.net/ja/q/b232f3)