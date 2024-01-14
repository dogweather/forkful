---
title:                "C: テキストファイルの書き方"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ書くのか

テキストファイルを書く理由はさまざまです。例えば、データを保存したい場合や、プログラムの出力を保存したい場合に使うことができます。

## 書き方

テキストファイルを書く方法は簡単です。まず、ファイルを開きます。次に、書き込みモードでファイルを開く必要があります。最後に、```fprintf()```関数を使用してテキストをファイルに書き込みます。

```
#include <stdio.h>
int main()
{
  FILE *fp;
  fp = fopen("sample.txt", "w");
  fprintf(fp, "こんにちは、世界!");
  fclose(fp);
  return 0;
}
```

上記のコードを実行すると、"sample.txt"という名前のファイルが作成され、その中に"こんにちは、世界!"というテキストが書き込まれます。

## 詳細を掘り下げる

テキストファイルを書く際には、いくつかの注意点があります。まず、ファイルが正しく開かれているかどうかを確認する必要があります。ファイルが開かれないと、エラーが発生します。また、ファイルに書き込むデータの型に注意する必要があります。文字列を書き込む場合は、```fprintf()```関数の第二引数に%sというフォーマット指定子を使用します。

## 詳しくはこちらを参照してください

- [C プログラミング（初心者ユーザー向け）](https://www.geeksforgeeks.org/c-programming-language/)
- [C言語 ファイル入出力 - pat Suite ～ プログラミング入門 ～](https://www.tohoho-web.com/ex/cpp.html#file2)
- [C言語ファイル入出力テストドライバ突撃インタビュー - Qiita](https://qiita.com/kotetu/items/0531c1856b6ad2c7f355)

## 参考リンク

- [Markdownの書き方 - Qiita](https://qiita.com/kamorits/items/6f342da395ad57468ae3)
- [Markdownチートシート - Qiita](https://qiita.com/Qiita/items/c686397e4a0f4f11683d)