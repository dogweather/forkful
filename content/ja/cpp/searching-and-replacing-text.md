---
title:                "C++: テキストの検索と置き換え"
simple_title:         "テキストの検索と置き換え"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ 

コンピュータプログラミングにおいて、テキストの検索と置換はとても重要な作業です。例えば、膨大な量のデータを扱う場合、特定のキーワードを含む文字列を探し出して置き換えることで、効率的にデータを更新することができます。

## 使い方 

テキストの検索と置換を行うためには、C++プログラミング言語を使用することができます。まず、`ifstream`を使用してファイルを開きます。次に、`while`ループを使用してファイルから一行ずつ読み取り、`find`関数を使用して検索文字列を含むかどうかをチェックします。もし検索文字列が含まれていれば、`replace`関数を使用して置換を行います。最後に、ファイルを`ofstream`を使用して保存します。

````C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream inFile;
    inFile.open("input.txt");

    std::string line, searchWord, replaceWord;
    std::cout << "Enter the word to be searched: ";
    std::cin >> searchWord;
    std::cout << "Enter the word to be replaced: ";
    std::cin >> replaceWord;

    while (std::getline(inFile, line)) {
        if (line.find(searchWord) != std::string::npos) {
            line.replace(line.find(searchWord), searchWord.length(), replaceWord);
        }
        std::cout << line << std::endl;
    }

    inFile.close();
    return 0;
}
````

例えば、`input.txt`には次のような内容が含まれているとします。

```
こんにちは。私の名前はユウです。
私はコンピュータサイエンスを勉強しています。
```

もし検索文字列として「私」、置換文字列として「僕」を入力した場合、次のような出力が得られます。

```
こんにちは。僕の名前はユウです。
僕はコンピュータサイエンスを勉強しています。
```

このように、テキストの検索と置換を行うことで、必要な情報を簡単に更新することができます。

## 詳細を調べる 

C++では、`find`関数と`replace`関数を使用することでテキストの検索と置換を行うことができます。`find`関数は、検索文字列が最初に現れる位置を返し、見つからない場合は`-1`を返します。`replace`関数は、置換する文字列を指定された位置から指定された長さ分置き換えます。

また、文字列を扱う際には、`<string>`ヘッダファイルをインクルードする必要があります。さらに、`std::string::npos`は最大の値を表す定数であり、文字列中に検索文字列が見つからなかった場合に使用されます。

## 関連記事 

- [C++入門編 – テキストファイルを1行ずつ読み込んで出力する方法](https://qiita.com/tyoruyoutube/items/29ab7b7190b399db737f)
- [std::stringの関数一覧](https://cpprefjp.github.io/reference/string/string/functions.html)