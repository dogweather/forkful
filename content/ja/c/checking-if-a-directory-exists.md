---
title:                "ディレクトリの存在をチェックする"
html_title:           "C: ディレクトリの存在をチェックする"
simple_title:         "ディレクトリの存在をチェックする"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何 & なぜ?
ディレクトリが存在するかどうかを確認することは、プログラマーがプログラムを実行する前に必要な手順です。ディレクトリが存在しない場合、プログラムは意図したとおりに動作しない可能性があります。そのため、ディレクトリの存在を確認することは重要な課題です。

## 方法:
以下のコードブロックに示すように、C言語を使用してディレクトリの存在を確認する方法があります。

```C
#include <stdio.h>
#include <sys/stat.h>

int main() {
    char *directory = "example_directory";
    struct stat st = {0};
    
    if (stat(directory, &st) == -1) {
        printf("Directory does not exist.\n");
    } else {
        printf("Directory exists.\n");
    }
    
    return 0;
}
```

上記のコードでは、```stat()```関数を使用してディレクトリのメタデータを取得し、その結果を元にディレクトリが存在するかどうかを判断しています。もしディレクトリが存在しない場合、```stat()```関数は-1を返し、その結果を元にプログラムはディレクトリが存在しないと判断します。

## 詳細な情報:
ディレクトリの存在を確認する方法は、C言語以外でも利用可能です。Pythonなどのスクリプト言語では、```os.path.exists()```関数を使用することでディレクトリの存在を確認することができます。

ディレクトリの存在を確認するために使用される```stat()```関数は、POSIX標準で定義されており、LinuxやUnixなどの多くのオペレーティングシステムで利用可能です。

## 関連リンク:
- [C言語のstat()関数ドキュメント](https://www.ibm.com/support/knowledgecenter/en/ssw_ibm_i_74/rtref/stat.htm)
- [Pythonのos.path.exists()関数ドキュメント](https://docs.python.org/3/library/os.path.html#os.path.exists)