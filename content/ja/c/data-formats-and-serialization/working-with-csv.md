---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:55.878074-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.825148-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306E\u4E16\u754C\u3067\u3001\
  CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u30D5\u30A1\u30A4\u30EB\
  \u3092\u6271\u3046\u3068\u3001\u884C\u3054\u3068\u306B\u6574\u7406\u3055\u308C\u305F\
  \u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u306E\u30C7\u30FC\u30BF\
  \u306E\u8AAD\u307F\u53D6\u308A\u3068\u66F8\u304D\u8FBC\u307F\u304C\u542B\u307E\u308C\
  \u307E\u3059\u3002\u5404\u884C\u306F\u30EC\u30B3\u30FC\u30C9\u3092\u8868\u3057\u3001\
  \u5404\u30EC\u30B3\u30FC\u30C9\u306E\u30D5\u30A3\u30FC\u30EB\u30C9\u306F\u30AB\u30F3\
  \u30DE\u3067\u533A\u5207\u3089\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u305D\u306E\u5E83\u7BC4\u306A\u30B5\u30DD\u30FC\u30C8\u3068\u8868\
  \u5F62\u5F0F\u306E\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306E\
  \u7C21\u6F54\u3055\u306E\u305F\u3081\u3001\u3055\u307E\u3056\u307E\u306A\u30B7\u30B9\
  \u30C6\u30E0\u9593\u306E\u30C7\u30FC\u30BF\u306E\u30A4\u30F3\u30DD\u30FC\u30C8/\u30A8\
  \u30AF\u30B9\u30DD\u30FC\u30C8\u306E\u5BB9\u6613\u3055\u306E\u305F\u3081\u306B\u3001\
  CSV\u30D5\u30A1\u30A4\u30EB\u3092\u64CD\u4F5C\u3057\u307E\u3059\u3002."
title: "CSV\u30D5\u30A1\u30A4\u30EB\u306E\u64CD\u4F5C"
weight: 37
---

## 方法：


### CSVファイルの読み取り
C言語でCSVファイルを読み取るには、文字列の操作関数と一緒に標準ファイルI/O関数を使用し、各行を解析します。以下は、CSVファイルを読み取り、各行のフィールドをコンソールに出力する基本的な例です。

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("ファイルを開けません\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *field = strtok(buf, ",");
        while(field) {
            printf("%s\n", field);
            field = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
サンプル `data.csv`:
```
Name,Age,Occupation
John Doe,29,Software Engineer
```

サンプル出力:
```
Name
Age
Occupation
John Doe
29
Software Engineer
```

### CSVファイルへの書き込み
同様に、CSVファイルに書き込むことは、コンマ区切り形式でデータを保存するために `fprintf` を使用することを意味します。

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("output.csv", "w");
    if (!fp) {
        printf("ファイルを開けません\n");
        return 1;
    }

    char *headers[] = {"Name", "Age", "Occupation", NULL};
    for (int i = 0; headers[i] != NULL; i++) {
        fprintf(fp, "%s%s", headers[i], (headers[i+1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Jane Doe", 27, "Data Scientist");

    fclose(fp);
    return 0;
}
```

サンプル `output.csv` の内容:
```
Name,Age,Occupation
Jane Doe,27,Data Scientist
```

## 深い潜入
CSV形式は、一見すると単純ですが、フィールド内のカンマの処理やフィールドを引用符で囲むなどのニュアンスが含まれています。示された基本的な例は、そのような複雑さを考慮せず、また、潜在的なエラーを堅牢に処理するわけでもありません。

歴史的に、C言語でのCSV処理は、言語の低レベルな性質とこのようなタスクのための組み込みの高レベルの抽象化の欠如のため、主に手動で行われてきました。この手動管理には、ファイルのオープン、行の読み取り、文字列の分割、必要に応じたデータタイプの変換が含まれます。

C言語でCSVファイルを直接操作することは、ファイルI/Oや文字列の扱いに関する貴重な学習体験を提供しますが、`libcsv`や`csv-parser`のようなライブラリは、引用符で囲まれたフィールドやカスタム区切り文字のサポートを含む、CSVファイルの読み書きのための包括的な機能を提供し、効率とエラープロセスの少ないプロセスを約束します。

代わりに、それをサポートするエコシステム内で作業する場合、高レベルのCSV操作機能を提供する言語やプラットフォーム（Pythonの`pandas`ライブラリのように）と統合することは、CSV処理が多く必要なアプリケーションのためのより生産的なルートになることがあります。このクロス言語アプローチは、C言語のパフォーマンスとシステムプログラミングの能力を活用しつつ、CSV処理などの特定のタスクに対して他の言語の使いやすさを利用します。
