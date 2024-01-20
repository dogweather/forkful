---
title:                "正規表現の使用"
html_title:           "Bash: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何となぜ？

正規表現とは、一連の文字列をマッチングするためのパターンです。プログラマーがこのテクニックを使用するのは、コード内の特定の文字列を見つけたり、検証したり、置き換えたりするためです。

## 使い方：

正規表現を使用するC++の基本的な例を以下に示します。

```C++
#include <regex>
#include <iostream>

int main() {
    std::string s = "I love 123 programming languages";
    std::regex e ("\\d+");  // 注意：数字を表す正規表現

    // s内の数字がマッチするかどうか判断
    if (std::regex_search (s,e))
        std::cout << "There are some digits in the string\n";
}
```
このコードを実行すると、「There are some digits in the string」と表示されます。このメッセージは、指定したパターン("\\d+")が文字列の中で見つかったことを示しています。

## 深掘り：

正規表現は、1950年代に開発されたもので、UNIXパイプラインのgrepコマンドなど、初期のコンピューティング環境で一般的に使用されていました。それらは現在でもその価値が認められ、あらゆる高レベルプログラミング言語に採用されています。

一方で、正規表現の代わりにワイルドカードを使用することも可能ですが、正規表現の方がはるかに強力であるため、より複雑なパターンマッチングが求められる場合には正規表現を用いることが一般的です。

また、C++の正規表現は、内部的には無限オートマトンとして実装されています。これにより、さまざまな複雑なパターンマッチングを効率良く行うことが可能となります。

## 関連情報：

以下のリンクでは、正規表現の詳細や使い方、さらに進んだトピックなどについて学ぶことができます。

- C++のregexライブラリ（https://www.cplusplus.com/reference/regex/）
- 正規表現のチュートリアル（https://regexone.com/）
- 正規表現クイックリファレンス（http://www.rexegg.com/regex-quickstart.html）