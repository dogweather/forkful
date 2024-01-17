---
title:                "テキストファイルの読み込み"
html_title:           "C++: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

お帰りなさい！今日はテキストファイルの読み込みについてお話ししましょう。プログラマーにとって、テキストファイルの読み込みは重要なスキルです。それでは始めましょう！

## What & Why?
テキストファイルの読み込みとは、テキストデータ（文字や数字など）をコンピューターが扱える形式に変換することを指します。プログラマーはテキストファイルを読み込むことで、プログラムに必要なデータを簡単に取得し、処理することができます。

## How to:
テキストファイルを読み込むには、まずファイルを開く必要があります。ファイルを開くには、C++の```fstream```ライブラリを使用します。以下のコードを参考にしてください。

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main()
{
    // ファイルを開く
    ifstream file;
    file.open("sample.txt");

    // ファイルからデータを読み込む
    string data;
    while (getline(file, data)) {
        cout << data << endl;
    }

    // ファイルを閉じる
    file.close();

    return 0;
}
```

上記のコードでは、```file```オブジェクトを使用してファイルを開き、```getline()```関数を使用してファイルからデータを取得しています。取得したデータは、```cout```を使用して表示することができます。

以下は、```sample.txt```が以下のような内容の場合の出力結果です。

```
こんにちは
私の名前は太郎です
```

```
こんにちは
私の名前は太郎です
```

## Deep Dive:
テキストファイルの読み込みは、コンピューターの歴史がさかのぼる１９７０年代から行われてきました。当時は、データの入出力が主にテキストファイルを通じて行われていました。しかし、現在ではデータベースやAPIなどの新しい技術が登場し、テキストファイルの使用は減少しています。

テキストファイルの代わりに、より高度なデータ形式（例：XMLやJSON）を使用し、より多くの情報を含めることができるようになりました。また、インターネットの普及により、テキストデータをオンライン上で扱うことが主流となりました。

## See Also:
- [C++でのファイルの開き方](https://www.sejuku.net/blog/4126)
- [C++のfstreamライブラリ](https://www.cplusplus.com/reference/fstream/)