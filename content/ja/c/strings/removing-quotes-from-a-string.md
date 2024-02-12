---
title:                "文字列から引用符を削除する"
aliases:
- /ja/c/removing-quotes-from-a-string/
date:                  2024-02-03T18:07:29.951272-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/removing-quotes-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となく理由

C言語で文字列から引用符を削除するとは、単一（' '）または二重（" "）の引用符で囲まれたテキストコンテンツを抽出することを意味します。このプロセスは、入力データのサニタイジング、ファイル内容の解析、または引用符が不要であるか、データ処理でエラーを引き起こす可能性がある場面で文字列をさらに処理する準備をするために不可欠です。

## 方法:

C言語で文字列から引用符を削除するには、引用符以外の文字だけを新しい文字列にコピーしながら文字列を走査します。このプロセスは、文字列に存在する引用符すべてを削除するか、あるいは先頭と末尾の引用符だけを削除するかに合わせて調整することができます。以下に示す例は、これらのアプローチの両方を示しています:

```c
#include <stdio.h>
#include <string.h>

// 文字列からすべての引用符を削除する関数
void removeAllQuotes(char *source, char *dest) {
    while (*source) {
        if (*source != '"' && *source != '\'') {
            *dest++ = *source;
        }
        source++;
    }
    *dest = '\0'; // 目的の文字列をヌル終端
}

// 文字列から先頭と末尾の引用符だけを削除する関数
void removeEdgeQuotes(char *source, char *dest) {
    size_t len = strlen(source);
    if (source[0] == '"' || source[0] == '\'') source++, len--;
    if (source[len-1] == '"' || source[len-1] == '\'') len--;
    strncpy(dest, source, len);
    dest[len] = '\0'; // 目的の文字列をヌル終端
}

int main() {
    char str1[] = "'Hello, World!'";
    char str2[] = "\"Programming in C\"";
    char noQuotes1[50];
    char noQuotes2[50];
    
    removeAllQuotes(str1, noQuotes1);
    printf("All Quotes Removed: %s\n", noQuotes1);
    
    removeEdgeQuotes(str2, noQuotes2);
    printf("Edge Quotes Removed: %s\n", noQuotes2);
    
    return 0;
}
```
サンプル出力:
```
All Quotes Removed: Hello, World!
Edge Quotes Removed: Programming in C
```

これらの例は、文字列に存在するすべての引用符の削除と、先頭と末尾の引用符だけの目標とした削除の両方をどのように扱うかを示しています。

## 深掘り

文字列から引用符を削除するという概念は、C言語において、初期のテキスト処理のニーズに結びついている以外に、顕著な歴史的深みがない。ここで示された直接的なアプローチは万能ですが、非常に大きい文字列や高性能を要求される場合、その場での修正やより高度なアルゴリズムが好まれる場合があり、効率性に欠けます。

`strpbrk`を使用して引用符を見つけ、引用符でない部分の文字列を移動するような代替方法はより効率的かもしれませんが、C言語でのポインターとメモリ管理についての深い理解を要求します。さらに、正規表現ライブラリの出現は、引用符の削除を含む文字列操作のための強力なツールセットを提供してきました。しかしながら、これらのライブラリは強力ですが、単純なタスクには必要のない複雑さとオーバーヘッドを加えるかもしれません。そのため、示された直接的なアプローチは、単純さと多くの一般的な用途での効果を組み合わせた、Cプログラマーにとって価値のあるスキルのままです。
