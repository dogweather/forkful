---
title:                "C: 正規表現を使用する"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使用する理由は様々ですが、主な目的は文字列のパターンを簡単に検索・置換することです。これにより、プログラミングの効率性が向上し、複雑な操作を簡単に行うことができます。

## 使い方

正規表現を使用するためには、まず```#include <regex.h>```を宣言する必要があります。次に、パターンの作成とコンパイルを行います。例えば、任意の数字にマッチするパターンを作成する場合、```regex_t pattern;```のように変数を宣言し、```regcomp(&pattern, "[0-9]", 0);```でコンパイルします。その後、マッチさせたい文字列を```regexec(&pattern, "sample123", 0, NULL, 0)```のように記述します。この場合、```"sample123"```は上記のパターンにマッチしているので、処理が成功し、```REG_NOMATCH```のようなエラーコードが返されることはありません。

```
#include <stdio.h>
#include <regex.h>

int main()
{
    regex_t pattern;
    int result;
    char *str = "sample123";

    result = regcomp(&pattern, "[0-9]", 0);

    if (result == 0)
    {
        result = regexec(&pattern, str, 0, NULL, 0);

        if (result == 0)
        {
            printf("%sはマッチしています。\n", str);
        }
        else if (result == REG_NOMATCH)
        {
            printf("%sはマッチしていません。\n", str);
        }
    }

    return 0;
}
```
実行結果：
```
sample123はマッチしています。
```

## 深く掘り下げる

正規表現をより効率的に利用するためには、パターンの作成にあたっては「量指定子」の使用や「キャプチャグループ」の活用などに注目する必要があります。また、エスケープシーケンスや後方参照などの様々な機能もありますので、正規表現の詳細を学ぶことが重要です。

## 参考記事

- [正規表現入門 - Qiita](https://qiita.com/jnchito/items/893c887fbf19e17d3ff9)
- [Cで正規表現利用 - 日々の記録](http://ushitora.net/archives/910)
- [C言語で正規表現を使う方法 - HELLO IT LAB](https://hello-itlab.com/c-2102)