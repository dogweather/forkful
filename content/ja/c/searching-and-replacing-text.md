---
title:    "C: テキストを検索して置換する"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ？

テキストを検索して置換することのメリットは何でしょうか？例えば、大量のテキストファイルやコードを編集する際に、手作業で1つずつ変更するのはとても大変です。しかし、検索と置換を使えば、簡単に一括で変更できます。これにより、時間を節約し、ミスを防ぐことができるのです。

## 方法

C言語でテキストの検索と置換を行うには、正規表現という機能を使います。まず、対象となるテキストを読み込んで、任意の文字列を検索して、別の文字列に置換するプログラムを作成します。以下は簡単な例です。

```C
#include <stdio.h>
#include <string.h>

int main() {

    // テキストを読み込む
    char text[100] = "Hello world!";
    
    // "world"を"Japan"に置換する
    char* result = strstr(text, "world");
    if (result) {
        strncpy(result, "Japan", strlen("Japan"));
    }
    
    // 結果を表示する
    printf("%s", text);
    
    return 0;
}
```

実行すると、"Hello Japan!"という結果が出力されます。

## 深く掘り下げる

C言語では、正規表現を使ってさまざまな検索と置換の方法があります。例えば、ワイルドカードを使って複数の文字列を一度に置換することもできます。また、大文字と小文字を区別しないで検索することもできます。

さらに、検索した文字列を変数に代入して、別の関数に渡すことも可能です。これにより、より柔軟なプログラムを作成することができます。

## See Also

- [正規表現の基礎](https://qiita.com/shuntaro_tamura/items/d38d1605dbf4c6153b4c)
- [C言語の文字列操作](https://www.javadrive.jp/c-language/string/index1.html)
- [文字列の置換プログラムの作り方](https://qiita.com/kymmt90/items/5ef373010451aba83e2b)