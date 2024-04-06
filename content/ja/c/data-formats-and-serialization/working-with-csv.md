---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:55.878074-07:00
description: "\u65B9\u6CD5\uFF1A C\u8A00\u8A9E\u3067CSV\u30D5\u30A1\u30A4\u30EB\u3092\
  \u8AAD\u307F\u53D6\u308B\u306B\u306F\u3001\u6587\u5B57\u5217\u306E\u64CD\u4F5C\u95A2\
  \u6570\u3068\u4E00\u7DD2\u306B\u6A19\u6E96\u30D5\u30A1\u30A4\u30EBI/O\u95A2\u6570\
  \u3092\u4F7F\u7528\u3057\u3001\u5404\u884C\u3092\u89E3\u6790\u3057\u307E\u3059\u3002\
  \u4EE5\u4E0B\u306F\u3001CSV\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u53D6\u308A\
  \u3001\u5404\u884C\u306E\u30D5\u30A3\u30FC\u30EB\u30C9\u3092\u30B3\u30F3\u30BD\u30FC\
  \u30EB\u306B\u51FA\u529B\u3059\u308B\u57FA\u672C\u7684\u306A\u4F8B\u3067\u3059\u3002"
lastmod: '2024-04-05T21:53:43.616466-06:00'
model: gpt-4-0125-preview
summary: ''
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
