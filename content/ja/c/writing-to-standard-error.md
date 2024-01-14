---
title:                "C: 「標準エラーへの書き込み」"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ 

標準エラー出力に書き込むことが重要なのでしょうか？プログラミングをする上で、標準エラー出力を理解することは重要なスキルです。標準エラー出力はエラーメッセージやデバッグ情報を表示するために使用されます。そのため、コードをデバッグしたい場合やエラーが発生した際に対処することができます。

## 使い方

標準エラー出力に書き込むには、```fprintf(stderr, "エラーメッセージ");```のようにコードを書きます。これは、標準エラー出力ストリームにエラーメッセージを書き込むことを意味します。また、エラーメッセージの中に変数の値を表示することもできます。例えば、```fprintf(stderr, "エラーが発生しました。エラーコードは%dです。", errorCode);```のように書けます。

以下のコードを実行すると、エラーメッセージが標準エラー出力に表示されます。

```C
#include <stdio.h>

int main() {
    int num = 5;
    fprintf(stderr, "エラー：数字%dは偶数です。", num);
    return 0;
}
```

上記のコードの出力結果は以下のようになります。

```bash
エラー：数字5は偶数です。
```

## ディープダイブ

標準エラー出力についてさらに深く掘り下げてみましょう。通常、Cプログラムでは```printf()```を使用して標準出力にデータを表示しますが、```fprintf()```は標準エラー出力にデータを表示することができます。これは、標準出力と標準エラー出力はそれぞれ異なるストリームであるためです。標準出力の内容はコンソールに表示され、標準エラー出力の内容は通常、ログファイルに書き込まれます。

また、標準エラー出力はリダイレクトすることもできます。例えば、```./a.out 2> log.txt```のようにコマンドを実行すると、エラーメッセージは標準エラー出力のストリームからログファイルにリダイレクトされます。これにより、コンソールの出力が見やすくなります。

## さらに読む

- [標準エラー出力について](https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_73/apis/con_stderr.htm)
- [標準エラー出力を使ったデバッグ方法](https://www.cprogramming.com/debugging/errors.html)
- [標準エラー出力のリダイレクトについての詳細](https://tldp.org/LDP/abs/html/io-redirection.html#SIMPLEERR)

## 参考

- [標準エラー出力を使ってデバッグしよう！](https://qiita.com/takubok/items/de8919c47f9f7b5fdbab)
- [C言語で標準エラー出力を使おう](https://dev.classmethod.jp/articles/c-stderr/)