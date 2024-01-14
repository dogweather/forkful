---
title:                "C++: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

パターンに一致する文字を削除することに興味があるのはなぜでしょうか？主にテキスト処理や文字列操作において、特定の文字を削除することで必要な情報のみを取得したり、処理をスムーズにするために行われます。

## 方法

パターンに一致する文字を削除する方法は様々ありますが、今回はC++言語を使用したコーディング例を紹介します。

まずは、stringクラスを使用してテキストを入力し、パターンに一致する文字を削除するプログラムを書いてみましょう。

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
   string text;
   string pattern;

   cout << "テキストを入力してください：";
   getline(cin, text);

   cout << "削除するパターンを入力してください：";
   cin >> pattern;

   size_t index = text.find(pattern); // パターンに一致する文字の位置を取得
   while (index != string::npos) { // 見つからない場合はstring::nposを返すのでループを抜ける
      text.erase(index, pattern.length()); // 見つかった位置からパターンの長さ分の文字を削除
      index = text.find(pattern, index); // 次のパターンに一致する文字を探す
   }

   cout << "削除後のテキスト：" << text << endl;

   return 0;
}
```

このコードでは、テキストを入力し、その中から削除するパターンを指定します。そして、stringクラスの`find()`メソッドを使用してパターンに一致する文字の位置を取得し、`erase()`メソッドを使用して削除しています。これを`while`ループで繰り返すことで、テキスト中のすべてのパターンに一致する文字を削除することができます。

もうひとつの方法として、正規表現を使用する方法もあります。`regex`ライブラリをインクルードし、`regex_replace()`メソッドを使用することで、より簡単にパターンに一致する文字を削除することができます。

```C++
#include <iostream>
#include <string>
#include <regex>

using namespace std;

int main() {
   string text;
   regex pattern;

   cout << "テキストを入力してください：";
   getline(cin, text);

   cout << "削除するパターンを入力してください：";
   cin >> pattern;

   string result = regex_replace(text, pattern, ""); // パターンに一致する文字を空文字に置換
   cout << "削除後のテキスト：" << result << endl;

   return 0;
}
```

上記の方法では、`regex_replace()`メソッドを使用することで、より簡単にパターンに一致する文字を削除することができます。

## ディープダイブ

パターンに一致する文字を削除する方法について深く掘り下げると、正規表現を使用する方法が最も柔軟で強力であることが分かります。正規表現を使用することで、特定の文字だけでなく、様々なパターンにも一致する文字を削除することができます。また、`regex`ライブラリを使用することで、多言語対応やパフォーマンスの向