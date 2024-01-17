---
title:                "正規表現を使う"
html_title:           "C++: 正規表現を使う"
simple_title:         "正規表現を使う"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
正規表現を使用するとは何でしょうか？それは、文字列のパターンを簡単に検索・置換・抽出するための方法です。プログラマーは、このような機能を利用して、コード内のテキストを効率的に処理することができます。

## 方法：
以下のように、C++で正規表現を使用する方法を示します。 ```C++
#include <regex>
using namespace std;

int main() {
  string text = "こんにちは、私の名前は太郎です。";
  regex pattern("私の名前は([a-zA-Z]+)です");
  smatch match;

  if (regex_search(text, match, pattern)) {
    cout << match.str(1) << endl; // 太郎 が出力されます
  }
}
```
上記のコードでは、文字列 `text` の中から、正規表現 `私の名前は([a-zA-Z]+)です` にマッチする部分を探し、マッチした部分の一つ目のグループを出力しています。

## 詳細：
正規表現は、1960年代から使われているパターンマッチングの手法です。他の言語にも同様の機能がありますが、C++の場合は `<regex>` ライブラリを利用することで実現できます。代替手段としては、文字列操作を行う関数を自作する方法もありますが、正規表現を使うことでより簡単かつ高速に処理できる可能性があります。

## 関連情報：
より詳しい使い方やアドバンスドなトピックについては、以下のリンクを参考にしてみてください。
- [正規表現チュートリアル (C++)](https://www.cplusplus.com/reference/regex/regex/)
- [正規表現オンラインテストツール](https://regex101.com/)
- [Boost Regex Library](https://www.boost.org/doc/libs/1_76_0/libs/regex/doc/html/index.html)