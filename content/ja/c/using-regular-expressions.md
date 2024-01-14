---
title:                "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# なぜ正規表現を使うのか

正規表現は、文字列を効率的に検索や比較するためのツールです。複雑なパターンを持つテキストを処理する際に非常に便利です。

## 使い方

正規表現を使うためには、まず `regex.h` ヘッダーファイルをインクルードする必要があります。次に、`regex_t` データ型を定義し、正規表現をコンパイルします。最後に、`regexec()` 関数を使って文字列とマッチするかどうかを確認します。

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    regcomp(&regex, "test", 0);

    char* string = "This is a test string";
    int result = regexec(&regex, string, 0, NULL, 0);

    if (result == 0) {
        printf("String contains 'test'\n");
    } else {
        printf("String does not contain 'test'\n");
    }

    regfree(&regex);

    return 0;
}
```

上記のコードは、文字列 `This is a test string` にパターン `test` が含まれているかどうかをチェックしています。`regexec()` 関数の戻り値が 0 の場合、文字列にマッチすると判断され、それ以外の場合はマッチしないと判断されます。

## 深堀り

正規表現には、様々な特殊文字があります。例えば、`^` は文字列の先頭を表し、`$` は文字列の末尾を表します。また、`[]` で囲われた文字列パターンは、そのいずれかの文字にマッチすることを示します。

正規表現は非常に強力なツールですが、パターンを正しく記述することが重要です。間違ったパターンで正規表現を使用すると、思わぬバグや予期しない結果が生じる可能性があります。より詳細な情報は、[ドキュメンテーション](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)を参照することをおすすめします。

## 参考リンク

- [正規表現チュートリアル](https://qiita.com/jnchito/items/893c887fbf19e17d3ff9)
- [正規表現の基礎](https://techacademy.jp/magazine/16081)
- [GNU C Libraryドキュメンテーション](https://www.gnu.org/software/libc/manual/html_node/index.html#Top)