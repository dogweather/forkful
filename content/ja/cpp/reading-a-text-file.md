---
title:                "C++: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# なぜテキストファイルを読み込むのか

テキストファイルを読み込むことは、プログラミングにおいて非常に重要です。例えば、大量のデータを処理する際に、テキストファイルを使用することで簡単にアクセスすることができます。また、ファイルから読み込んだデータをプログラムの実行中に変更することもできます。このようにテキストファイルを読み込むことは、プログラミングの様々な場面で役立ちます。

# 読み込み方の簡単な例

まずは、C++でテキストファイルを読み込む一番基本的な方法を見てみましょう。

```C++
#include <iostream>
#include <fstream> // ファイル入出力を扱うためのライブラリ

using namespace std;

int main() {
    // ファイルを開く
    ifstream file("example.txt");

    // ファイルが開けたかどうかをチェックする
    if (!file) {
        cerr << "ファイルが開けませんでした。" << endl;
        return 1;
    }

    // テキストファイルから1行ずつ読み込んで出力する
    string line;
    while (getline(file, line)) {
        cout << line << endl;
    }

    // ファイルを閉じる
    file.close();

    return 0;
}
```

上記のコードでは、"example.txt"という名前のファイルを開き、ファイルが開けたかどうかをチェックします。その後、テキストファイルから1行ずつ読み込んでコンソールに出力します。最後に、ファイルを閉じてプログラムを終了します。

## テキストファイル読み込みの詳細

上記の例では、テキストファイルを1行ずつ読み込んでいましたが、実際にはさまざまな方法でファイルを読み込むことができます。

例えば、ファイル全体を一度に読み込む方法や、特定の条件を満たす行だけを読み込む方法などがあります。また、ファイルへの書き込みやファイルの内容を変更する方法もあります。

さらに、ファイルの種類や大きさによっても読み込み方は異なります。プログラムの実行速度にも影響するため、最適な読み込み方法を選択することが重要です。

# また見てみよう

- [C++のファイル入出力について](https://cpprefjp.github.io/reference/fstream/)

- [C++のstringクラスについて](https://cpprefjp.github.io/reference/string/basic_string.html)

- [C++プログラミング入門](https://www.javadrive.jp/cpp/)