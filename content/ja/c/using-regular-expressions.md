---
title:    "C: 正規表現の使用"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# なぜ正規表現を使用するのか

正規表現は、文字列データを検索や置換する際に非常に有用なツールです。特定のパターンを持つ文字列を簡単に特定したり、難しい文字列操作を簡略化することができます。

## 使い方

正規表現を使用するには、まずC言語のヘッダーファイルである"regex.h"をインクルードする必要があります。その後、正規表現を扱うための構造体や関数を定義します。以下のようなサンプルコードをご覧ください。

```
#include <regex.h>
#include <stdio.h>

int main()
{
  // 正規表現を扱うための構造体を定義
  regex_t regex;

  // 正規表現にマッチするかどうかを判定するためのフラグを定義
  int flag;

  // パターンを含む文字列を定義
  char str[] = "Hello, World!";

  // 正規表現のパターンを定義
  char pattern[] = "Hello";

  // 正規表現をコンパイルする
  regcomp(&regex, pattern, 0);

  // 文字列に正規表現のパターンが含まれているかどうかを判定
  flag = (regexec(&regex, str, 0, NULL, 0) == 0) ? 1 : 0;

  if (flag) {
    printf("正規表現にマッチしました。\n");
  } else {
    printf("正規表現にマッチしませんでした。\n");
  }

  // 正規表現構造体を解放する
  regfree(&regex);

  return 0;
}
```

上のコードでは、"Hello"というパターンを含む文字列を検索しています。もし文字列にマッチする場合は"正規表現にマッチしました。"という出力が、マッチしない場合は"正規表現にマッチしませんでした。"という出力が得られます。

## 詳細を深く掘り下げる

正規表現には様々な特殊文字があります。例えば、"."は任意の一文字を表し、"^"は文字列の先頭を表します。これらの特殊文字を組み合わせることで、より複雑なパターンを作ることができます。

また、正規表現はパフォーマンスが重要なアプリケーションではなるべく使用を避けるべきです。なぜなら、正規表現を使用すると文字列の検索や置換を行うために多くの計算量が必要になり、アプリケーションのパフォーマンスに影響を与える可能性があるためです。

# 参考リンク

- [正規表現入門](https://www.boost.org/doc/libs/1_47_0/libs/regex/doc/html/boost_regex/ref/regex_note.html)
- [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)
- [正規表現のパフォーマンスについて](https://dzone.com/articles/regex-performance)
- [正規表現の公式ドキュメント](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)

# 他に見る