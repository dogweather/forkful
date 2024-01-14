---
title:    "C++: 正規表現を使用する"
keywords: ["C++"]
---

{{< edit_this_page >}}

## なぜ正規表現を使用するのか

正規表現は、テキスト処理やデータ検索に非常に有用です。特定の構文パターンを指定することで、複雑な文字列の処理が簡単になります。これにより、コードの記述量を減らし、効率的にデータを処理することができます。

## 正規表現の使い方

正規表現を使用するには、C++プログラムに「regex」ライブラリを含める必要があります。次に、正規表現オブジェクトを作成し、検索したいパターンを指定します。下の例では、文字列「Hello, World!」から"Hello"を検索しています。

```C++
#include <regex>

// 正規表現オブジェクトの作成
std::regex pattern("Hello");
// 検索対象の文字列
std::string str = "Hello, World!";
// 検索
std::smatch match;
std::regex_search(str, match, pattern);

// 結果を出力
if (match[0].matched) {
  std::cout << "マッチング結果: " << match[0] << std::endl;
}

// 出力結果: マッチング結果: Hello
```

## 正規表現の深層へ

正規表現には、様々な機能があります。例えば、特定の文字を無視したり、繰り返し検索を行ったりすることができます。また、グループを作成してマッチング結果を取得したり、置換を行ったりすることも可能です。正規表現の詳細な使い方やオプションについては、ドキュメントを参照することをおすすめします。

## 参考になる情報

- [C++ regexライブラリのドキュメント](https://en.cppreference.com/w/cpp/regex)
- [正規表現を使いこなすための練習問題集](https://www.learncpp.com/cpp-tutorial/regular-expressions/)
- [正規表現の基礎知識と実践例](https://dev.to/danvandersluis/regex-tutorial-for-beginners-4gnn)