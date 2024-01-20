---
title:                "CSV ファイルを操作する"
html_title:           "C: CSV ファイルを操作する"
simple_title:         "CSV ファイルを操作する"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-csv.md"
---

{{< edit_this_page >}}

# CSV (Comma Separated Values) ファイルの扱い方

## 何としているの？
CSVはコンマで区切られたテキストファイルのことです。プログラマーがCSVを使う理由は、データベースやスプレッドシートといったデータフォーマットとの簡単なデータ交換や編集ができるからです。

## 使い方：
CSVファイルを読み込む最も一般的な方法は、データが保存されている行を一つずつ読み込んでから、カンマで分割して必要なデータを取得することです。次のように書けます：

```
FILE *fp; //ファイルポインタ
char buffer[256]; //データを一時的に保存するバッファ

//CSVファイルを開く
fp = fopen("sample.csv", "r");

//データを行ごとに読み込む
while (fgets(buffer, sizeof(buffer), fp)) {
    //データをカンマで分割
    char *token = strtok(buffer, ",");

    //必要なデータを取得
    char *name = token;
    token = strtok(NULL, ","); //次のデータを取得
    int age = atoi(token);

    //取得したデータを使用する
    printf("名前：%s, 年齢：%d\n", name, age);
}

//ファイルを閉じる
fclose(fp);
```

出力結果：

```
名前：田中太郎, 年齢：27
名前：山田花子, 年齢：32
```

## 深く掘り下げる：
CSVは1972年にIBMによって開発され、データベースとの相互運用性を考えて設計されました。現在では、ExcelやGoogle Sheetsなどのスプレッドシートソフトでもよく使われています。代替方法としては、XMLやJSONなどのフォーマットがありますが、CSVはシンプルで扱いやすいため、依然として広く使われています。

CSVファイルを作成するときに注意することは、データのフォーマットに一貫性を持たせることです。特に文字列に含まれるカンマや改行文字に注意する必要があります。また、UTF-8などの文字コードも考慮する必要があります。

## 関連情報：
- [RFC 4180 (CSVフォーマットの標準仕様)](https://www.ietf.org/rfc/rfc4180.txt)
- [CSVファイルを扱うライブラリ (libcsv)](http://libcsv.sourceforge.net/)