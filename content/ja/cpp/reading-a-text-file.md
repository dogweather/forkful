---
title:    "C++: テキストファイルの読み込み"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ
テキストファイルを読み込むことの利点は、プログラマーがデータを手動で入力する代わりに、ファイルから情報を取得できることです。これにより、より簡単に大量のデータを処理することができます。

## 方法
テキストファイルを読み込むには、C++の標準ライブラリである`fstream`を使用します。例えば、`input.txt`というファイルには`1, 2, 3`という数字が1行に書かれています。以下のコードを使用すると、その数字を読み取って出力することができます。

```C++
#include <iostream> 
#include <fstream> 
using namespace std; 

int main() { 
   ifstream input("input.txt"); 
   int num; 
   while(input >> num) { 
      cout << num << endl; 
   } 
   return 0; 
}
```

出力:
```
1
2
3
```

## 深堀り
テキストファイルを読み込む際には、いくつかの重要なポイントに注意する必要があります。まず、ファイルを開くときは`ifstream`を使用し、読み込みモードで開く必要があります。また、ファイルを読み取るときは、空白や改行を無視することができる`>>`演算子を使用します。また、ファイルの最後まで読み込んだ後は、ファイルを閉じることを忘れないようにしましょう。これらのポイントを抑えることで、テキストファイルから正しくデータを読み取ることができます。

## 参考リンク
- [C ++のテキストファイルの読み取りと書き込み](https://programmingterminology.com/ja/basic/file/cplusplus_file)
- [c ++ fstreamとifstreamファイル入力](https://zenn.dev/miko/engineer/articles/f66913fd47ca1803d729)
- [C ++のファイル入力-教訓ファイルを読み取る方法](https://www.guru99.com/cpp-file-input-output.html)

## 関連項目
- [Markdown形式の解説](https://ja.wikipedia.org/wiki/Markdown)
- [C ++の基礎](https://www.cplusplus.com/doc/tutorial/)
- [テキストファイル入力の利点](https://www.w3schools.in/cplusplus-tutorial/file-input-output/)