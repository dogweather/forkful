---
title:                "日付を文字列に変換する"
aliases:
- ja/c/converting-a-date-into-a-string.md
date:                  2024-02-03T17:54:15.751761-07:00
model:                 gpt-4-0125-preview
simple_title:         "日付を文字列に変換する"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-date-into-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Cで日付を文字列に変換することは、日付構造またはタイムスタンプを人が読み取れる形式に翻訳することを含みます。プログラマーは、ログ、ユーザーインターフェースに日付を表示したり、JSONやCSVのようなテキストベースの形式で日付を保存する場合に、しばしばこの作業を実行します。

## 方法:

この目的のために一般的に使用されるのは、`<time.h>`ライブラリからの`strftime`関数です。これにより、形式指定子を指定することで、さまざまな方法で日付と時刻を形式化できます。こちらが簡単な例です:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // 日付と時刻を文字列に変換（例: "Wed Jun 30 21:49:08 2021"）
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("現在の日付と時刻: %s\n", dateStr);
    return 0;
}
```

サンプル出力は次のようになるかもしれません:

```
現在の日付と時刻: Wed Jun 30 21:49:08 2021
```

`strftime`に渡される形式指定子を変更することで、形式をカスタマイズできます。例えば、日付を`YYYY-MM-DD`の形式で取得するには、`"%Y-%m-%d"`を使用します。

## 深掘り

`strftime`関数と`<time.h>`ライブラリは、C標準ライブラリの一部であり、元々のANSI C標準（C89/C90）にさかのぼります。このアプローチは多くのプラットフォームでサポートされており、直接的であるものの、より直感的な日付と時刻のライブラリを備えた現代のプログラミング言語と比べると、低レベルで面倒に見えるかもしれません。

C標準ライブラリの時間関数は広くサポートされており比較的簡単に使用できるものの、新しい言語のライブラリや国際化機能を備えたサードパーティCライブラリ（例：International Components for Unicode (ICU)）に見られるような複雑なタイムゾーン操作や国際化機能には欠けています。

それでも、`strftime`関数のカスタマイズ能力と広範なプラットフォームサポートは、Cでの日付文字列変換において信頼でき、有益なツールであることを意味します。より高レベルのdatetimeライブラリから来たプログラマーは、その低レベルな性質に慣れる必要があるかもしれませんが、多様なアプリケーションでの日付と時刻のフォーマットに際して、それが非常に強力で多様性があることを見いだすでしょう。
