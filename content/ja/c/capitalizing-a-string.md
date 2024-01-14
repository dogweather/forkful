---
title:    "C: 文字列のキャピタライズ"
keywords: ["C"]
---

{{< edit_this_page >}}

## なぜ
文字列を大文字に変換することに興味があるかもしれません。特定の文字列の書式を変更する必要がある場合や、表示を統一するために使用することができます。

## 使い方
文字列を大文字に変換する方法はいくつかありますが、今回は以下の方法を紹介します。

```C
#include <stdio.h>
#include <string.h>

int main()
{
  // 変換する文字列を指定
  char str[20] = "apple";
  
  // 変換した結果を保存するための新しい文字列を定義
  char new_str[20];
  
  // strの長さを取得
  int len = strlen(str);
  
  // 変換の処理を実行
  for (int i=0; i<len; i++)
  {
    // 大文字に変換する
    new_str[i] = toupper(str[i]);
  }
  
  // 新しい文字列を出力
  printf("変換後の文字列: %s", new_str);
  
  return 0;
}
```

上記のコードを実行すると、以下のような結果が得られます。

```
変換後の文字列: APPLE
```

## 詳細について
文字列を大文字に変換する際には、特定の文字をASCII形式で表すことが重要です。C言語では、文字を表すために整数値を使用するため、大文字に変換したい場合はその整数値を使って処理を行います。また、文字列内に数字や記号などが含まれている場合も注意が必要です。

## 他にも見てみる
この記事ではC言語で文字列を大文字に変換する方法を紹介しましたが、他にもさまざまな言語で同じような処理を行うことができます。

- [Pythonで文字列を大文字に変換する方法](https://programmar.jp/blog/python-uppercase-lowercase/)
- [Javaで文字列を大文字に変換する方法](https://targeting-japan.com/entry/toUpperCase-toLowerCase)
- [C#で文字列を大文字に変換する方法](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.toupper)

詳しいプログラムの動作や使い方については、各言語のドキュメントを参考にしてください。