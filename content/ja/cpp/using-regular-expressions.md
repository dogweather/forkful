---
title:                "「正規表現を使う」"
html_title:           "C++: 「正規表現を使う」"
simple_title:         "「正規表現を使う」"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

なぜ私たちは正規表現を使うのか？それは、テキスト内の特定のパターンや文の構造を自動的に検索、抽出、変更するためです。プログラマーにとっては、効率的で便利なツールとなっています。

## 使い方

正規表現を使う最も基本的な方法は、文字列を検索することです。例えば、文字列内に特定の単語が含まれているかどうかを確認したり、複数のバリエーションの単語を一度に検索することができます。

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
    string s = "Hello, world!";
    regex regex_search("world");
    if (regex_search.search(s)) {
        cout << "Found world in string!" << endl;
    }
    return 0;
}
```

上記の例では、`regex`クラスを使用して文字列を検索し、`cout`を使ってその結果を表示しています。もちろん、より複雑な検索パターンを定義することもできます。

```C++
regex regex_search("h[eo]llo"); // 'hello'または'hallo'にマッチ
regex regex_search("[A-Z][a-z]+"); // 最初の文字が大文字の英単語にマッチ
```

また、正規表現を使って文字列内のパターンを抽出することもできます。

```C++
string s = "Today is 2020/06/30";
regex regex_extract("\\d{4}\\/\\d{2}\\/\\d{2}"); // 日付のパターンにマッチ
smatch matches;
if (regex_search(s, matches, regex_extract)) {
    cout << matches.str() << endl; // 結果: 2020/06/30
}
```

正規表現を使うことで、より高度な検索や抽出が可能になります。例えば、電話番号やメールアドレス、URLなどの特定のパターンを正確に検出することができます。

## 深堀り

C++では、標準ライブラリの`<regex>`ヘッダーを使用して正規表現を扱うことができます。`regex`クラスの他にも、`smatch`クラスや`match_results`クラスを使用して、検索結果を取得することもできます。

また、C++では正規表現の構文が少し異なるため、他のプログラミング言語で習得した正規表現の知識がそのまま使えるとは限りません。しかし、標準ライブラリのドキュメントを参照することで、必要な構文を簡単に覚えることができます。

## 参考文献

- C++正規表現チュートリアル: https://www.cplusplus.com/reference/regex/
- RegexOne: Learn Regular Expressions with simple, interactive exercises: https://regexone.com/
- 正規表現の基礎: https://www.geeksforgeeks.org/introduction-to-regular-expressions-in-c/
- [この記事のサンプルコード](https://gist.github.com/example)