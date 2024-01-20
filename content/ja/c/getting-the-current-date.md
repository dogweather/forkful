---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 現在の日付の取得：C言語による実装ガイド

## 何と何のため？

現在の日付の取得は、システムの現在時刻を日付形式で示す処理です。これにより、日付や時間ベースの重要な操作（ログ生成、エンティティのタイムスタンプ付けなど）をプログラムで制御できます。

## 実装方法：

```C
#include <stdio.h>
#include <time.h>

int main(){
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);

    printf("今日の日付:%d-%d-%d ", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);

    return 0;
    }
```

サンプルの出力：

```C
今日の日付:2022-3-10 //現在の日時に応じて結果が変わります
```

## ディープダイブ

1. **歴史的な背景**: C言語は1970年代初めにUnixシステムのために開発されました。そのため、`time.h`ライブラリがUNIX時間（UNIXエポック）で日時を管理するための複数の関数を提供しています。UNIX時間は、1970年1月1日0時00分00秒（UTC）から現在までの経過秒数です。

2. **代替方法**: POSIX準拠のシステムでは、`gettimeofday`関数を使用しても日時を取得できますが、この関数は非推奨とされ、将来的には廃止される可能性があります。

3. **実装に関する詳細**: `time()`関数は現在のカレンダー時間を秒単位で返し、`localtime()`関数はその秒数をローカル時間に変換します。次に、`printf`を使って年、月、日を表示します。注意が必要な点として、年は1900年から開始し、月は0から開始しますので、適切な日付表示の為に年には1900を加え、月には1を加えています。

## 参照リンク：

- [公式C11規格ドキュメント](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf)
- [time.h](https://en.cppreference.com/w/c/chrono)
- [`gettimeofday`](http://man7.org/linux/man-pages/man2/gettimeofday.2.html)関数についての詳細