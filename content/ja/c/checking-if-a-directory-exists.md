---
title:    "C: ディレクトリが存在するかを確認する"
keywords: ["C"]
---

{{< edit_this_page >}}

## 説明

ファイルやフォルダを扱うプログラムを作る際、特定のディレクトリが存在するかどうかを確認する必要がある場合があります。このブログ記事では、C言語を使用してディレクトリの存在をチェックする方法を紹介します。

## 方法

ディレクトリの存在をチェックするには、`<unistd.h>`ヘッダーファイルの`access()`関数を使用します。`access()`関数は、ファイルやディレクトリにアクセスするための権限があるかどうかをチェックすることができます。以下のコード例を参考にしてください。

```C
#include <stdio.h>
#include <unistd.h>

int main() {
    // チェックするディレクトリのパスを指定
    char *path = "/Users/username/Documents/";

    // ディレクトリのアクセス権限を確認
    if (access(path, F_OK) == 0) {
        // 指定したパスのディレクトリが存在する場合の処理
        printf("%sディレクトリが存在します。\n", path);
    } else {
        // 指定したパスのディレクトリが存在しない場合の処理
        printf("%sディレクトリが存在しません。\n", path);
    }

    return 0;
}
```

上記のコードを実行すると、以下のような結果が表示されます。

```
/Users/username/Documents/ディレクトリが存在します。
```

また、`access()`関数の第2引数には、以下の定数を指定することもできます。

- `R_OK`: 読み取り権限をチェックする
- `W_OK`: 書き込み権限をチェックする
- `X_OK`: 実行権限をチェックする

それぞれの権限がある場合には、`== 0`の代わりに`!= 0`を使用して条件を書き換えることができます。

## 深堀り

`access()`関数は、ファイルやディレクトリの存在をチェックするだけでなく、権限も同時にチェックすることができます。また、エラーハンドリングを行うことで、失敗した場合の詳細なエラーメッセージを表示することも可能です。

また、`<sys/stat.h>`ヘッダーファイルの`stat()`関数を使用することでも、ファイルやディレクトリの存在を確認することができます。この関数を使用すると、ファイルやディレクトリの種類やサイズ、最終アクセス日時などの情報を取得することができます。詳細は、`man 2 stat`コマンドでマニュアルを確認してください。

## 参考リンク

- [access(2) - Linux man page](https://linux.die.net/man/2/access)
- [stat(2) - Linux man page](https://linux.die.net/man/2/stat)
- [C プログラミング - ファイルの読み書き (access / stat)](https://www.yukun.info/blog/2009/11/c-file-access-stat.html)

## 関連情報

[See Also]  
- [研究会メモ | access ()関数の使い方](https://www.slis.tsukuba.ac.jp/~fujisawa.makoto.fu/cgi-bin/wiki/index.php?access()+%A5%A4%A5%D9%A5%F3