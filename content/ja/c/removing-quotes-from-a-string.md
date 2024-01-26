---
title:                "文字列から引用符を削除する"
date:                  2024-01-26T03:38:12.108611-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から引用符を取り除くとは、文字列の内容の一部となっているシングル（''）またはダブル（""）の引用符を削除することを意味します。プログラマーはこれを行って、入力を無害化したり、データをさらなる処理のために準備したり、文字列を区切るために引用符を使用する言語でファイルパスやコマンドを扱う際の構文エラーを避けたりします。

## 方法：

ここに、文字列から邪魔な引用符を取り除くためのC関数を示します：

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Original: %s\n", str);
    remove_quotes(str);
    printf("Sanitized: %s\n", str);
    return 0;
}
```

サンプル出力：

```
Original: He said, "Hello, 'world'!"
Sanitized: He said, Hello, world!
```

## 詳細分析

文字列から引用符を取り除くという課題は、プログラミングの夜明け以来存在しており、データの衛生がエラーを避ける鍵（SQLインジェクション攻撃など）であり、引用符を制御文字と混同しがちなシステムに文字列を安全に渡すことができるようにすることが重要です。

歴史的には、異なる言語がこの課題を異なる方法で扱っています - 一部の言語には組み込み関数（Pythonの「strip」のような）がありますが、Cのような他の言語は、開発者に低レベル制御を提供することに焦点を当てているため、手動で実装する必要があります。

代替方法としては、`strpbrk`のようなライブラリ関数を使用して引用符を見つけたり、より複雑なパターンの場合はPCREのようなライブラリを使用した正規表現を使用することもできますが、単に引用符を取り除くためには過剰な場合があります。

上記の実装は、文字列内の各文字を単純にスキャンし、引用符でない文字のみを書き込みポインタの位置にコピーします。これは結果の文字列に追加のメモリが必要なく、その場で行われるため効率的です。

## 参照

- [C標準ライブラリ関数](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl互換正規表現](https://www.pcre.org/)
- [Cにおけるポインターの理解](https://www.learn-c.org/en/Pointers)
- [Cにおけるセキュアコーディング](https://owasp.org/www-project-secure-coding-in-c)