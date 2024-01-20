---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付を文字列に変換するとは、純粋な数字の日付 (例えば、2020/12/31) を人間が理解しやすい文字列 (例えば、'2020年12月31日') に変換することです。プログラマーがこれを行う理由は、主にユーザーが日付を理解しやすくするためです。

## どうやって：

以下は、strftime関数を使って日付を文字列に変換する例です：

```C
#include <stdio.h>
#include <time.h>

int main() {
  char buffer[50];
  time_t rawtime;
  struct tm * timeinfo;

  time(&rawtime);
  timeinfo = localtime(&rawtime);

  strftime(buffer, 50, "%Y年%m月%d日 - %H:%M", timeinfo);
  printf("Formatted date & time : %s\n", buffer);

  return 0;
}
```
このコードは日付と時間を次のような形式で出力します:

```
Formatted date & time : 2022年05月15日 - 16:40
```
## ディープダイブ：

Cにおける日付の文字列変換は、1970年代から存在しています。当時の主なツールは`asctime`と`ctime`でしたが、フォーマットが固定されていました。だからこそ、より柔軟な`strftime`が出現しました。

別のやり方として、sprintf関数を使って直接フォーマットする方法があります。しかし、この方法の問題点は、引数やフォーマット指定子の数が多くなるとエラーを見逃しやすくなることです。

`strftime`は効率的に日付を文字列に変換するために開発されました。ただし、この関数はlocaleに依存しています。つまり、実行する環境によって出力が異なる可能性があります。

## 参考文献：

- [`strftime`関数の公式ドキュメンテーション](https://en.cppreference.com/w/c/chrono/strftime)
- [`localtime`関数の公式ドキュメンテーション](https://en.cppreference.com/w/c/chrono/localtime)