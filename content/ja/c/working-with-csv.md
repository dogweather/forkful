---
title:                "「CSVの操作」"
html_title:           "C: 「CSVの操作」"
simple_title:         "「CSVの操作」"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを使うのか
CSVは、データを表形式で保存するための一般的なフォーマットです。多くのプログラミング言語でサポートされており、データ処理やデータ分析などの用途に利用されています。

## 方法
CSVをC言語で処理する方法を説明します。まずは、CSVファイルを読み込んでデータを保存する方法を見てみましょう。

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // CSVファイルを開く
    FILE *fp = fopen("sample.csv", "r");

    // ファイルが正しく開けたかをチェック
    if (fp == NULL)
    {
        printf("ファイルが見つかりません。\n");
        return 1;
    }

    // ファイルから1行ずつ読み込む
    char line[256];
    while (fgets(line, sizeof(line), fp) != NULL)
    {
        // カンマで区切られたデータを分割する
        char *token = strtok(line, ",");
        while (token != NULL)
        {
            // データを処理する
            printf("%s\n", token);

            // 次のデータへ移動
            token = strtok(NULL, ",");
        }
    }

    // ファイルを閉じる
    fclose(fp);

    return 0;
}
```

このコードを実行すると、CSVファイルのデータを1つずつ取得して表示することができます。

次に、CSVファイルにデータを書き込む方法を見てみましょう。

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // CSVファイルを開く
    FILE *fp = fopen("output.csv", "w");

    // ファイルが正しく開けたかをチェック
    if (fp == NULL)
    {
        printf("ファイルを作成できません。\n");
        return 1;
    }

    // 書き込むデータを準備する
    char *data = "1,2,3,4,5";

    // ファイルに書き込む
    fputs(data, fp);

    // ファイルを閉じる
    fclose(fp);

    return 0;
}
```

このコードを実行すると、指定したデータがCSVファイルに書き込まれます。

## 詳細を掘り下げる
CSVファイルには、文字列や数値だけでなく、日付や時刻などのデータを含むこともあります。そのような場合は、データを適切に処理するために、データ型の変換を行う必要があります。また、CSVファイルにはヘッダー行が含まれる場合もありますが、これを無視する方法も学ぶことができます。

さらに、CSVファイルを処理する際には、ファイルのエンコーディングやセパレーターなどの設定を行う必要があります。これらの設定を間違えると、データの読み込みや書き込みがうまく行われず、エラーが発生する場合もあります。

## さらに見る
- [C言語でCSVを扱う方法](https://programming.org.ua/jp/how-to-handle-csv-files-in-c/)
- [C言語のファイル操作 - ファイルの読み書き](https://programming.org.ua/jp/c-file-operations