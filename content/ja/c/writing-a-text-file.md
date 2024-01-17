---
title:                "テキストファイルの書き方"
html_title:           "C: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 何となぜ?
テキストファイルを書くというのは、コンピューターの中にテキストを保存することです。プログラマーは、コードやデータをテキストファイルに保存することで、バックアップや共有が簡単になります。

## 方法:
下に示すように、テキストファイルを書くにはいくつかの方法があります。使用する関数や構文については、Cの[公式ドキュメント](https://www.cprogramming.com/tutorial/cfileio.html)を参照してください。

```C
// ファイルを書き込みモードで開く
FILE *fp = fopen("sample.txt", "w");

// fprintfを使用してテキストをファイルに書き込む
fprintf(fp, "Hello, World!");

// ファイルを閉じる
fclose(fp);
```

上記のコードでは、`fprintf`関数を使って"Hello, World!"というテキストを`sample.txt`という名前のファイルに書き込んでいます。

## ディープダイブ:
テキストファイルの書き込みは、大昔からコンピューターの機能の一部として使われてきました。他のプログラミング言語でも同様の機能がありますが、Cのような低レベル言語では特に重要です。

また、テキストファイルの他にもバイナリファイルを書き込むこともできます。バイナリファイルでは、テキストファイルとは異なる特殊な形式でデータが保存されます。

テキストファイルは読み書きが簡単ですが、バイナリファイルの方がより高度な処理が必要になることがあります。どちらが適しているかは、使用するデータやプログラムの目的によって異なります。

## 関連情報:
- [C プログラミングチュートリアル](https://www.cprogramming.com/tutorial/cfileio.html)
- [テキストファイルとバイナリファイル](https://ja.wikipedia.org/wiki/ファイル形式#テキストファイルとバイナリファイル)