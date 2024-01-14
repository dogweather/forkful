---
title:                "C++: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

「なぜ」という疑問：なぜC++プログラマーは文字列を小文字に変換する必要があるのか、その理由をご紹介します。

##なぜ

文字列を小文字に変換することには、いくつかの利点があります。例えば、文字列を比較する場合には大文字と小文字を区別しない方が便利です。また、文書を整形する際や、特定の文字列を検索する際にも、小文字に変換することでより効率的な処理が可能になります。

##やり方

文字列を小文字に変換するには、C++の標準ライブラリである「<algorithm>」を使用します。具体的なコードは以下の通りです。

```C++
#include <iostream>
#include <algorithm>
#include <string>
using namespace std;

int main() {
  string str = "Hello, WORLD!";
  transform(str.begin(), str.end(), str.begin(), ::tolower);
  cout << str << endl;

  return 0;
}
```

上記のコードを実行すると、以下のような出力が得られます。

```
hello, world!
```

##詳細

C++の ```transform``` 関数は、```<algorithm>``` ヘッダーファイルで定義されており、文字列やコンテナなどのシーケンスを変換する際に非常に便利です。第3引数には変換したい文字列の開始位置を、第4引数には変換後の文字列の開始位置を、第5引数には変換関数を指定することができます。上記の例では、```::tolower``` という変換関数を使用し、開始位置を ```str.begin()``` と指定しています。

また、文字列を小文字に変換する方法はほかにもあります。例えば、ループを使用する方法や、一文字ずつ処理する方法などがあります。性能や使用するメモリ量などの観点から、最適な方法を選択することが大切です。

##詳細はこちら

- C++のtransform関数について詳しくは[こちら](https://en.cppreference.com/w/cpp/algorithm/transform)
- C++のアルゴリズムについては[こちら](http://www.cplusplus.com/reference/algorithm/)
- 文字列を変換する他の方法については[こちら](https://www.tutorialspoint.com/how-to-convert-string-to-lowercase-or-uppercase-in-c-cplusplus)

##関連リンク

- [C++の標準ライブラリについて深く探求する](https://www.ibm.com/developerworks/jp/views/linux/libraryview.jsp?search_by=c%2B%2B+stl)
- [効率的にC++を学ぶ方法](https://devlights.hatenablog.com/entry/2015/01/31/011448)
- [プログラミング初心者へのアドバイス](https://programmer-start.com/study/c-plus-plus/about-c-plus-plus/)