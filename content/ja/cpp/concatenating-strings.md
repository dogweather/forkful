---
title:                "C++: 「文字列の連結」"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の結合をするのには、多くの理由があります。一つの理由は、複数の情報を一つの変数にまとめるために使われることです。また、同じメッセージを再利用したり、より複雑な文を作るためにも使われます。

## 使い方

結合は、単純な代入演算子（+=）を使って実行することができます。文字列同士を直接結合することもできますが、変数を使っても結合することができます。以下の例を参考にしてください。

```C++
#include <iostream>
using namespace std;

int main() {
  // 文字列の結合
  string greeting = "こんにちは";
  string name = "太郎";
  string message = greeting + "、" + name + "さん！";
  cout << message << endl; // 出力： こんにちは、太郎さん！

  // 単純代入演算子を使った結合
  string sentence = "今日は";
  sentence += "いい天気ですね。";
  cout << sentence << endl; // 出力： 今日はいい天気ですね。

  return 0;
}
```

## 深堀り

C++では、文字列を扱う際に ```string``` というデータ型が使われます。このデータ型は、可変長の文字列を格納することができます。文字列の結合は、このデータ型が持つ ```+``` 演算子や ```+=``` 代入演算子を使って実行することができます。

また、最新のC++17では、文字列の結合に ```std::string_view``` という新しいデータ型が使用されます。このデータ型を使うことで、メモリの節約やパフォーマンスの向上が可能になります。

## 参考リンク

- [C++ string concatenation](https://www.learncpp.com/cpp-tutorial/c-string-concatenation/)
- [std::string](https://aoki2.si.gunma-u.ac.jp/CommandRef/C/string.html)
- [C++17: The next great landing C++ standard](https://www.modernescpp.com/index.php/c-17-the-next-great-landing-c-standard)

## かも知れない

GitHubでは、C++で書かれたプロジェクトやコードを多く見ることができます。是非、チェックしてみてください！