---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
日付のパースとは、文字列から日付情報を取り出すプロセスのことを指します。これは、さまざまなフォーマットの日付を統一し、要求に応じて効率的に使用できるようにするためにプログラマーが行います。

## 使い方：
以下に示すように、`sscanf`関数の利用で、文字列から日付をパースします。

```C
#include <stdio.h>
int main() {
    int day, month, year;
    char str[] = "15-09-2021";
    sscanf(str, "%d-%d-%d", &day, &month, &year);
    printf("Day: %d, Month: %d, Year: %d\n", day, month, year);
    return 0;
}
```

このコードを実行すると以下のような出力が生じます：
```
Day: 15, Month: 9, Year: 2021
```

## 深堀り：
### ヒストリカルコンテキスト
早い段階では、日付の解析は手動で行われ、それぞれの文脈で異なる方法が使用されました。 Cプログラミング言語が提供する一般的な関数のひとつとしての`sscanf`は、標準化と自動化を提供しました。

### 代替案：
一部のプロジェクトでは、特殊なニーズを満たすためにカスタムパーサーを使用することがあります。また、他の言語やライブラリには、より深いコントロールまたはより豊富な機能を提供する日付パーサがあります。

### 実装の詳細：
Cにおける`sscanf`の基本的な機能では、「%d」は整数を検索し、「-」は区切り文字として機能します。この関数は文字列を左から右へと読み進み、指示に従って値を抽出します。

## 参考資料：
- Cプログラミングにおける[`sscanf`](https://www.cplusplus.com/reference/cstdio/sscanf/)
- 日付と時間についての[Cプログラミングチュートリアル](https://www.programiz.com/c-programming/library-function/time.h/strptime)
- [`strftime`](https://www.cplusplus.com/reference/ctime/strftime/)によるCにおける日付フォーマットのカスタマイズ。