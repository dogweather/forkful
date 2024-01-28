---
title:                "エラー処理"
date:                  2024-01-26T00:49:35.400479-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/handling-errors.md"
---

{{< edit_this_page >}}

## 何となぜ？
C言語におけるエラー処理は、予想外のことを想定することについてです。これにより、問題に遭遇したときにプログラムが暴走するのを防ぎます。プログラマーはエラーを洗練された方法で処理し、コードを信頼性の高いものに保つためにこの処理を行います。

## 方法：

C言語でこれを行う方法を見てみましょう：

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("nonexistentfile.txt", "r");
    if (fp == NULL) {
        perror("ファイルを開ける際のエラー");
        return EXIT_FAILURE;
    }
    // ファイルを何かしら処理
    fclose(fp);
    return EXIT_SUCCESS;
}
```

ファイルが存在しないときのサンプル出力：
```
ファイルを開ける際のエラー: そのようなファイルやディレクトリはありません
```

## 深掘り

初期のC言語におけるエラー処理は基本的なものでした - 主に戻り値と手動チェックがほとんどでした。組み込み変数`errno`は、関数が失敗したときに更新されます。ただし、それ自体ではスレッドセーフではありませんので、より良いエラー報告のために後に`strerror`や`perror`関数が導入されました。

代替手段？現代のC言語は`errno`に限定されていません。大変な問題が発生したときに非局所ジャンプするためのsetjmpやlongjmpがあります。一部の人々は独自のエラーコードを定義することを好み、他の人々はC++の例外処理のような構造を選択します。

実装の詳細は複雑になることがあります。たとえば、POSIX準拠のシステムではスレッドローカルストレージ（TLS）の魔法により`errno`はスレッドセーフです。組み込みシステムではリソースが貴重なため、ソフトウェアが肥大化する可能性がある標準的なアプローチよりもカスタムのエラー処理コードが好まれることがあります。

## 参照

- `errno`に関する詳細な解説：https://en.cppreference.com/w/c/error/errno
- スレッド安全とPOSIXスレッドとerrnoについて：http://man7.org/linux/man-pages/man3/pthread_self.3.html
- setjmpとlongjmpの紹介：https://www.cplusplus.com/reference/csetjmp/
- C++における例外処理についてはこちら：https://isocpp.org/wiki/faq/exceptions
