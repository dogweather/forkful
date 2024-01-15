---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# なぜ: パターンに一致する文字を削除することに関わる、効果的な理由が最大2文で説明される。

削除に関連するパターンを見つける上での最も一般的な理由の一つは、データの前処理です。具体的には、特定のパターンを持つ文字を削除することで、データをより扱いやすく、分析しやすくすることができます。また、それ以外の理由としては、文字の置換や変換のために使用される場合もあります。例えば、文章内の全ての小文字を大文字に変換するために、正規表現を使用して一致する文字を削除することができます。

## 使い方:
以下のコード例を参考にして、パターンに一致した文字を削除する方法を学びましょう。

```C
#include <stdio.h>
#include <string.h>

void deleteChar(char *str, char pattern) {
    int i, j = 0;
    for (i = 0; str[i] != '\0'; i++) {
        if (str[i] != pattern) {
            str[j++] = str[i];
        }
    }
    str[j] = '\0';
}

int main() {
    char str[] = "Hello, World!";
    char pattern = 'o';

    printf("Before deletion: %s\n", str);
    deleteChar(str, pattern);
    printf("After deletion: %s\n", str);

    return 0;
}
```

出力:

```
Before deletion: Hello, World!
After deletion: Hell, Wrld!
```

この例では、文字列から'o'というパターンに一致する文字を削除しました。これにより、元の文字列には変更を加えずに、新しい文字列を作成することができました。また、パターンとして1文字だけでなく、正規表現を使用して複数の文字列を削除することもできます。

## 深堀り:
パターンに一致する文字を削除する方法には、多くのアプローチがあります。上のコード例では、一致しない文字を新しい文字列にコピーすることで、削除を実現しました。しかし、これ以外の方法としては、文字列をループし、特定の文字をスキップすることもできます。また、メモリを効率的に管理するために、新しい文字列を確保する代わりに、既存の文字列内で削除を行うこともできます。

更に、パターンに一致した全ての文字を削除するのではなく、最初の一致する文字だけを削除する方法も考えられます。これにより、文字列内に複数の同じパターンが存在する場合でも、最初の一致する文字だけを削除することができます。

# 参考:
- [Tutorialspoint: Regular Expressions in C](https://www.tutorialspoint.com/c_standard_library/c_function_regexp.htm)
- [GeeksforGeeks: Strings in C](https://www.geeksforgeeks.org/strings-c-2/)
- [RegexOne: Learn Regular Expressions](https://regexone.com/)

See Also:
- [Markdown Guide: Basic Syntax](https://www.markdownguide.org/basic-syntax/)
- [Markdown Cheat Sheet](https://guides.github.com/pdfs/markdown-cheatsheet-online.pdf)