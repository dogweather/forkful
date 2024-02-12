---
title:                "テキストファイルの読み込み"
aliases:
- ja/cpp/reading-a-text-file.md
date:                  2024-01-20T17:54:03.117695-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ファイルからテキストを読み込むっていうのは、そのまま文字通りの意味で、プログラムがテキストファイルのデータを取り込むことです。この方法を使う理由は、設定、データのインポート、ログの分析など多岐にわたります。

## How to: (方法)
```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("example.txt");
    if (!file.is_open()) {
        std::cerr << "ファイルを開けませんでした！" << std::endl;
        return 1;
    }

    std::string line;
    while (getline(file, line)) {
        std::cout << line << std::endl;
    }

    file.close();
    return 0;
}
```
テキストファイル`example.txt`にそれぞれ異なる内容の行があるとします。 すると上のコードの出力は、ファイル内のそれぞれの行を新しい行で出力します。

## Deep Dive (深掘り)
ファイルからテキストを読むのは、初期のプログラミングからある基本的な操作です。`<fstream>`はC++においてストリームを通したファイルI/Oを扱うためのライブラリです。古い代替手段にはCの`stdio.h`がありますが、C++では型安全性や例外処理などの利点から`<fstream>`が推奨されています。

ライブラリのバックグラウンドは、読み取り操作を抽象化し、データを順不同に扱えるようにしています。そのため、ファイルからデータ取り込みの方法はシンプルですが、効率や例外処理などの重要な側面も考慮する必要があります。

## See Also (関連情報)
- [C++のファイル入出力チュートリアル](https://cplusplus.com/doc/tutorial/files/)
- [Stack Overflow: C++でのテキストファイルの読み書き](https://stackoverflow.com/questions/tagged/c%2b%2b+file-io)
