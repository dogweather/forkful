---
date: 2024-01-20 17:54:03.117695-07:00
description: "\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u30C6\u30AD\u30B9\u30C8\u3092\u8AAD\
  \u307F\u8FBC\u3080\u3063\u3066\u3044\u3046\u306E\u306F\u3001\u305D\u306E\u307E\u307E\
  \u6587\u5B57\u901A\u308A\u306E\u610F\u5473\u3067\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u304C\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u30C7\u30FC\u30BF\u3092\
  \u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u3053\u306E\u65B9\u6CD5\u3092\
  \u4F7F\u3046\u7406\u7531\u306F\u3001\u8A2D\u5B9A\u3001\u30C7\u30FC\u30BF\u306E\u30A4\
  \u30F3\u30DD\u30FC\u30C8\u3001\u30ED\u30B0\u306E\u5206\u6790\u306A\u3069\u591A\u5C90\
  \u306B\u308F\u305F\u308A\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.578791-06:00'
model: gpt-4-1106-preview
summary: "\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u30C6\u30AD\u30B9\u30C8\u3092\u8AAD\
  \u307F\u8FBC\u3080\u3063\u3066\u3044\u3046\u306E\u306F\u3001\u305D\u306E\u307E\u307E\
  \u6587\u5B57\u901A\u308A\u306E\u610F\u5473\u3067\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u304C\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u30C7\u30FC\u30BF\u3092\
  \u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u3053\u306E\u65B9\u6CD5\u3092\
  \u4F7F\u3046\u7406\u7531\u306F\u3001\u8A2D\u5B9A\u3001\u30C7\u30FC\u30BF\u306E\u30A4\
  \u30F3\u30DD\u30FC\u30C8\u3001\u30ED\u30B0\u306E\u5206\u6790\u306A\u3069\u591A\u5C90\
  \u306B\u308F\u305F\u308A\u307E\u3059\u3002."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

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
