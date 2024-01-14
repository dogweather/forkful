---
title:    "C++: 文字列の連結"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# なぜ、文字列の連結をするのか

プログラマーの皆さん、こんにちは！今日はC++で文字列の連結（concatenation）についてお話しします。文字列の連結とは、複数の文字列を結合して、1つの大きな文字列にすることを指します。例えば、"Hello"と" world"を連結すると、"Hello world"という1つの文字列になるわけです。

では、なぜ私たちは文字列の連結をするのでしょうか？それは、プログラムで扱うデータが必ずしも1つの塊であるとは限らないからです。例えば、ユーザーから入力された名前と姓を組み合わせて、フルネームを表示したい場合など、複数の文字列を結合する必要があります。また、プログラムで作成した文章をファイルに保存する際、連結された文字列を1つの行として扱うこともあります。

## どのようにして文字列を連結するのか

それでは、実際にC++で文字列の連結を行う方法を見ていきましょう。以下のように、```+```演算子を使用することで複数の文字列を連結することができます。

```C++
// 文字列の連結の例
#include <iostream>
using namespace std;

int main() {
  string greeting = "Hello";
  string name = " world";
  string phrase = greeting + name;
  cout << phrase; // Hello worldと出力される
  return 0;
}
```

また、```+=```演算子でも文字列を連結することができます。例えば、上記のコードを以下のように書き換えることもできます。

```C++
string phrase = greeting;
phrase += name;
```

それぞれの方式によって、出力結果は同じになります。

## ディープダイブ：文字列の連結についてさらに詳しく

文字列の連結には他にも様々な方法があります。例えば、```append()```関数や```insert()```関数を使用することで文字列を連結したり、挿入したりすることができます。また、文字列の長さを取得するための```length()```関数や、特定の文字から始まるかどうかを判定する```starts_with()```関数なども利用できます。

また、文字列の連結が何回も繰り返し行われる場合、効率的に処理を行うためには、```stringstream```クラスや```ostringstream```クラスなどを使用することもできます。

文字列の連結に関するさらに詳しい情報は、C++のドキュメントを参照することをおすすめします。

## See Also（関連リンク）

- [C++における文字列の連結の方法](https://www.geeksforgeeks.org/concatenating-two-strings-in-cpp/)
- [C++ドキュメント：文字列の操作](https://en.cppreference.com/w/cpp/string/basic_string)

それでは、今日の記事が皆さんのC++プログラミングにお役立ちでありますように。ありがとうございました！