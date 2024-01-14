---
title:    "C: ディレクトリが存在するかどうかをチェックする"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## もし、ディレクトリが存在するかどうかをチェックするのか

ディレクトリが存在するかどうかをチェックすることは、プログラミングにおいて非常に重要なことです。なぜなら、プログラムが正しく動作するためには、特定のフォルダが存在することが前提条件となるからです。そのため、ディレクトリの存在をチェックすることは、プログラムをより堅牢にするために欠かせない作業なのです。

## 方法: プログラミングの例と出力

ディレクトリの存在をチェックするためには、C言語の ```opendir()``` と ```closedir()``` の関数を使用します。例えば、以下のコードを参考にしてください。

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    struct dirent *dptr;
    DIR *dir = opendir("my_directory"); // チェックしたいディレクトリ名を指定
    if (dir == NULL) { // ディレクトリが存在しない場合
        printf("ディレクトリは存在しません\n");
    } else { // ディレクトリが存在する場合
        printf("ディレクトリが存在します\n");
        closedir(dir); // ディレクトリを閉じる
    }
    return 0;
}
```
出力結果:
```
ディレクトリが存在します
```

このように、ディレクトリの存在をチェックすることができます。

## 深堀り: ディレクトリの存在をチェックする方法

ディレクトリの存在をチェックするには、基本的に以下の手順を踏みます。

1. ```opendir()``` 関数を使ってディレクトリを開く。
2. もし ```opendir()``` が ```NULL``` を返した場合、ディレクトリは存在しないと判断する。
3. ディレクトリが存在する場合は、そのディレクトリを読み込んだ後、```closedir()``` 関数を使ってディレクトリを閉じる。

また、ディレクトリの存在をチェックする方法は、プログラムの実行環境によって異なる場合があります。そのため、プログラムを書く前に、使用する関数やライブラリのドキュメントを確認することが重要です。

## 参考リンク

- [C言語 - ディレクトリ存在チェック - opendirとclosedir](https://www.yoheim.net/blog.php?q=20111003)
- [C言語のファイル操作](https://pioneel.hatenablog.com/entry/20090417/1240013229)
- [opendir() function in C](https://www.geeksforgeeks.org/opendir-call-in-c-with-examples/)

## 参考文献

- [データ構造とアルゴリズム - C言語による実装と理解](https://www.amazon.co.jp/dp/4822245908/ref=cm_sw_r_tw_dp_j3LsFbPM27A7T?language=en_US)