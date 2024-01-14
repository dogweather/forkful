---
title:                "C: ディレクトリの存在を確認する"
simple_title:         "ディレクトリの存在を確認する"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# なぜディレクトリが存在するかをチェックする必要があるのか

ディレクトリが存在するかどうかをチェックすることは、ファイルの読み取りや書き込みを行う際に非常に重要です。ディレクトリが存在しない場合、ファイル操作がエラーを引き起こす可能性があります。そのため、事前にディレクトリの存在を確認することは、プログラムの正常な動作を保証するために必要な手順です。

## ディレクトリの存在をチェックする方法

ディレクトリが存在するかどうかを確認するには、以下のようなC言語のコードを使用します。

```
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>

int main(){
    // チェックするディレクトリのパスを指定
    char *path = "/Users/username/Documents";
    
    // stat()関数を使用してファイルの情報を取得
    struct stat buffer;
    int status = stat(path, &buffer);
    
    // ディレクトリが存在しない場合、stat()関数は-1を返す
    if (status == -1){
        printf("指定されたディレクトリは存在しません");
    } else {
        printf("指定されたディレクトリは存在します");
    }
    
    return 0;
}
```

上記のコードを実行すると、指定されたディレクトリの存在を確認することができます。

```
## ディープダイブ

ディレクトリが存在するかどうかをチェックするために使用されるstat()関数は、UnixやLinuxなどのオペレーティングシステムにおいて、ファイルの情報を取得するためによく使用される関数です。また、ディレクトリの存在を確認するためには、stat()関数の他にもaccess()関数やopendir()関数なども利用することができます。これらの関数を組み合わせることで、より複雑なディレクトリのチェックを行うことができます。

# 参考リンク

- [stat()関数のマニュアル](https://linuxjm.osdn.jp/html/LDP_man-pages/man2/stat.2.html)
- [access()関数のマニュアル](https://linuxjm.osdn.jp/html/LDP_man-pages/man2/access.2.html)
- [opendir()関数のマニュアル](https://linuxjm.osdn.jp/html/LDP_man-pages/man3/opendir.3.html)

## 関連リンク

- [ディレクトリチェック方法の簡単な解説動画](https://www.youtube.com/watch?v=jHc_PrVzM10)
- [C言語でディレクトリの存在を確認する方法](https://qiita.com/takuya-nakamura/items/a380dff8aabf3437e3b1)
- [C言語におけるファイル操作の基本](https://gihyo.jp/dev/serial/01/file_operate)