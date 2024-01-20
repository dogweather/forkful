---
title:                "CSVファイルの操作"
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
CSVは、Comma-Separated Valuesの略で、データをコンマで区切ったテキスト形式です。プログラマはCSVを使い、データ交換と保存をシンプルに行うために使います。

## How to: (方法)
```C
#include <stdio.h>

int main() {
    FILE *fp = fopen("sample.csv", "r");
    if (!fp) {
        printf("ファイルオープン失敗\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, sizeof(buf), fp)) {
        printf("%s", buf);
    }

    fclose(fp);
    return 0;
}
```
出力:
```
名前, 年齢, 都市
山田太郎, 30, 東京
佐藤花子, 25, 京都
```

## Deep Dive (深堀り)
CSV形式は1970年代初めに登場しました。JSONやXMLと異なり、人間にも機械にも読みやすい平文テキストです。C言語でのCSV処理は、`fopen`、`fgets`、`fscanf`、`strtok`関数などを使います。しかし、これらの関数は型の検証やエスケープ処理を自動で行わないため、注意が必要です。

## See Also (関連情報)
- [RFC 4180](https://tools.ietf.org/html/rfc4180): CSVフォーマットの仕様。
- [C Standard Library (libc)](https://en.wikipedia.org/wiki/C_standard_library): Cの標準ライブラリについての詳細。
- [GNU libcsv](http://sourceforge.net/projects/libcsv/): CSVファイルの読み書きを容易にするサードパーティのライブラリ。