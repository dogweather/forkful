---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列を小文字に変換するとは、与えられた文字列内の全ての大文字を対応する小文字に変換するプロセスのことです。プログラマーがこれを行う主な理由は、一般的に文字列の比較や検索を容易にするためです。

## 方法：
以下に小文字への変換を行うCプログラムのサンプルコードとその出力を示します。

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char text[] = "Hello, World!";
    
    for(int i = 0; text[i]; i++){
      text[i] = tolower(text[i]);
    }
    
    printf("%s\n", text);
    
    return 0;
}
```
このプログラムの出力は以下のとおりです:
```C
"hello, world!"
```

## 深堀り：
### 歴史的背景
小文字変換の概念は、コンピューティングが成長し、パフォーマンス要求が増すにつれて発展してきました。C言語は成熟した言語で、その設計が始まった1970年代からプログラマーがこのタスクを容易に実行できるようになりました。
### 代替案
 `tolower`関数以外にも、大文字を小文字に変換するためのいくつかの方法があります。ASCII値を直接操作することで実現可能ですが、`tolower`関数は綺麗で直感的なソリューションを提供します。
### 実装の詳細
上記のプログラムでは、`tolower`関数を使用して文字列を小文字に変換しています。この関数はctype.hライブラリに含まれており、大文字の文字が渡されると、それを対応する小文字に変換します。

## 参考情報
- [Wikipedia: Case Folding](https://en.wikipedia.org/wiki/Case_folding)
- [cplusplus.com: tolower](http://www.cplusplus.com/reference/cctype/tolower/)
- [ASCII Table: lower case conversion](https://asciitable.xyz)