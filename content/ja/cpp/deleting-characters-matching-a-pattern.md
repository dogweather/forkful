---
title:    "C++: パターンに一致する文字を削除する"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

パターンにマッチする文字を削除することのメリットとして、データの整形や不要な情報を取り除くことが挙げられます。

## 方法

パターンマッチングを行い、マッチした文字を削除するプログラム例を以下に示します。

```C++
// 例えば、文字列 "Hello, world!" から"llo"を削除する場合
#include <iostream>
#include <string>
using namespace std;

int main(){
    string str = "Hello, world!";
    string pattern = "llo";
    int pos = str.find(pattern);   // パターンにマッチする位置を取得
    str.erase(pos, pattern.size());   // マッチした文字を削除
    cout << str < < endl;   // 出力： He, world!
    return 0;
}
```

## 深堀り

パターンマッチングとは、特定の文字列に対するパターンを定義し、そのパターンに一致する文字列を検索・抽出することです。C++では、stringクラスのメソッドであるfind()とerase()を組み合わせることで、パターンにマッチする文字を削除することが可能です。

## 参考リンク

- [C++で文字列操作を行う方法](https://qiita.com/kaityo256/items/76bdf4e0a0503a78589f)
- [stringクラスのメソッドの仕様について](http://www.cplusplus.com/reference/string/string/)