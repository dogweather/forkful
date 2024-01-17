---
title:                "パターンに一致する文字の削除"
html_title:           "C++: パターンに一致する文字の削除"
simple_title:         "パターンに一致する文字の削除"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なに？なぜ？

文字列のパターンにマッチする文字を削除することは、プログラマーたちがよくやることです。これを行うのは、テキストの特定の部分を取り除きたいときや、無駄な文字を取り除いてデータを整えたいときに役立ちます。

## やり方：

### コード例１：

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
  string text = "こんにちは！さようなら！";
  // パターンにマッチする文字を削除する
  text.erase(remove(text.begin(), text.end(), '!'), text.end());

  cout << text << endl;
  // 出力結果：こんにちはさようなら
  return 0;
}
```

### コード例２：

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string text = "a1b2c3d4e5f6";
  // パターンにマッチする文字を削除する
  for (int i = 0; i < text.length(); i++) {
      if (text[i] >= '0' && text[i] <= '9') {
        text.erase(i, 1);
        i--;
      }
  }
  cout << text << endl;
  // 出力結果：abcdef
  return 0;
}
```

## ディープダイブ：

文字を削除する方法は、古くから存在しているテキスト処理の基本的なテクニックのひとつです。代替手段として、正規表現を使う方法もありますが、文字の削除はかんたんで効率的な方法です。文字をどのように削除したいかによって、様々な実装方法がありますが、基本的には文字列の操作を行う関数を使うことで文字を削除することができます。

## 関連ソース：

- [C++で正規表現を使う方法](https://www.ibm.com/docs/ja/i/7.2?topic=functions-regex-expression-cpp)
- [C++ string の erase 関数の使い方](https://www.jpcpp.org/reference/stl/erase/)