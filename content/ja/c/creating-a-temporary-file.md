---
title:                "C: 一時ファイルの作成"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##なぜ

一時ファイルを作成する理由は何でしょうか？一時ファイルの作成は、プログラマーにとって非常に重要な機能です。一時ファイルを作成することで、データを一時的に保存し、処理を継続することができます。また、一時ファイルを使用することで、データのオーバーフローやメモリの消費を防ぐことができます。

## 作り方

一時ファイルを作成する方法はいくつかありますが、最も基本的な方法は「tmpfile()」関数を使用することです。下記のコード例では、一時ファイルを作成し、その中に文字列を書き込み、最後に読み取る方法を示します。

```C
#include <stdio.h>

int main() {
   FILE *fp;
   char str[80];

   //一時ファイルを作成
   fp = tmpfile();

   //文字列を書き込む
   fputs("一時ファイルへようこそ！", fp);

   //ファイルを先頭に戻す
   rewind(fp);

   //一時ファイルから読み取る
   fgets(str, 80, fp);
   printf("読み取った文字列：%s\n", str);

   fclose(fp);  //ファイルを閉じる
   
   return 0;
}
```

上記のコードを実行すると、以下のような出力が得られるでしょう。

```
読み取った文字列：一時ファイルへようこそ！
```

## 徹底解説

一時ファイルを作成する方法には、他にも「mkstemp()」「tmpnam()」などがあります。また、一時ファイルを使用する際には、ファイルのモードやパスの指定など、さまざまなオプションが存在します。プログラマーにとっては重要な機能であるため、一時ファイルを作成する際には、しっかりとドキュメントを確認し、適切な方法を選択しましょう。

## 併せて読みたい

- [C言語 ファイル操作・一時ファイル作成](https://www.javadrive.jp/cstart/file/index8.html)
- [C言語 CPAN 詳細マニュアル - 一時ファイルの作成と管理](https://perldoc.perl.org/perlfunc.html#tmpfile)