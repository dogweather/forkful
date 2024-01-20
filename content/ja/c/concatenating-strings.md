---
title:                "文字列の連結"
html_title:           "PHP: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何と何故？
文字列の結合は、一つまたは複数の文字列を連結するプロセスです。このプロセスは、情報を動的に描画したり、ユーザー入力を生成したりするためにプログラミングで頻繁に行われます。

## 実装方法:
以下に`strcat`関数と`strncat`関数を使用して文字列を結合するCプログラムの例を示します。

```C
#include <stdio.h>
#include <string.h>

int main() {
    char string1[20] = "Hello ";
    char string2[10] = "World!";
    strcat(string1, string2);
    printf("%s\n", string1);
    
    char string3[20] = "Good ";
    char string4[10] = "Morning!";
    strncat(string3, string4, 7);
    printf("%s\n", string3);
    return 0;
}
```

上記のプログラムを実行すると以下の出力が得られます:

```
Hello World!
Good Morning
```
## 詳細説明:
過去には、文字列を連結するために独自の関数を作成し、ポインタ操作を駆使して実装することが一般的でした。しかし現在では、標準Cライブラリに含まれる`strcat`および`strncat`関数を利用するという方法が主流となっています。

この2つの関数は基本的に同じ動作をしますが、`strncat`関数は第三引数として連結する文字数を指定することができます。これにより、メモリオーバーフローを引き起こす可能性を低減できます。

もし、パフォーマンスを重視して効率的な文字列の連結を実装したい場合は、C言語の他にも、C++の`std::string`クラスやPythonの`join`関数などを活用できます。

## 参考資料:
以下に文字列の連結に関する詳細な情報を提供している資料のリンクを掲載します:
