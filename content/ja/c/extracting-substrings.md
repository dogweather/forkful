---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から部分文字列を抽出するとは、ある文字列から特定の部分を切り出すことを言います。プログラマーは、ほしい情報だけを取得したり、不要な部分を削除したりするためにこれを行います。

## 実行方法:

以下に部分文字列の抽出方法を示します。

```C 
#include <stdio.h>
#include <string.h>

int main() 
{
    char str[100] = "This is a Test.";
    char sub[100];

    // Extracts substrings from position 10, having length 4.
    strncpy(sub, str+10, 4);
    sub[4] = '\0';

    printf("%s\n", sub);  // Outputs: Test

    return 0;
}
```

このコードは、与えられた文字列から位置10から長さ4の部分文字列を抽出します。

## より深く：

* **歴史的な背景:**  部分文字列の抽出は、命令型プログラミング言語が始まる前から存在しています。これはクリップボード操作とも関連しています。
  
* **選択肢:** `strncpy` 関数以外にも、`sscanf`, `strtok`, `strspn`, `strcspn` 関数などの関数を使用して文字列を抽出できます。
  
* **実装の詳細:** `strncpy` 関数は、NUL-終端されていない文字列をコピーします。したがって、手作業でNUL文字を追加する必要があります。空のスペースは、コピーされる部分文字列の新しい終端となります。

## 関連項目：

* [Detailed Tutorial on strncpy](https://man7.org/linux/man-pages/man3/strncpy.3.html)
* [Details on Alternative Functions](https://www.tutorialspoint.com/c_standard_library/c_function_strspn.htm)
* [C Programming by WikiBooks](https://en.wikibooks.org/wiki/C_Programming/Strings)