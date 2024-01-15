---
title:                "「csvを利用する」"
html_title:           "Bash: 「csvを利用する」"
simple_title:         "「csvを利用する」"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを扱うのか

CSVはComma-Separated Valuesの略で、コンマで区切られたデータ形式のことを指します。このような形式でデータを保存することで、異なるプログラム間でデータをやりとりすることができるようになります。そのため、データ分析やデータ処理を行う際に非常に便利な形式となります。

## 手順

まずは、CSVファイルを作成します。ここでは、Bashプログラムを使ってCSVファイルを作成する方法を紹介します。以下のコードを使って、3列のデータを持つCSVファイルを作成します。

```Bash
#!/bin/bash

echo "Name, Age, Occupation" > data.csv
echo "John, 25, Engineer" >> data.csv
echo "Emily, 30, Accountant" >> data.csv
echo "Michael, 28, Designer" >> data.csv
```

上記のコードを実行すると、`data.csv`という名前の新しいファイルが作成され、3つのデータ行が含まれることになります。

次に、CSVファイルからデータを読み取り、データを処理して出力する方法を見ていきましょう。以下のコードを使って、`data.csv`からデータを読み取り、30歳以上の人のデータを表示するプログラムを実行します。

```Bash
#!/bin/bash

while IFS=, read -r name age occupation
do
	if [ $age -ge 30 ]
	then
		printf "%s is %d years old and works as a %s.\n" "$name" $age "$occupation"
	fi
done < data.csv
```
このプログラムでは、`IFS`コマンドを使ってコンマを区切り文字と指定し、`read`コマンドでCSVファイルからデータを読み取っています。そして、`if`文を使って30歳以上のデータをフィルタリングし、`printf`コマンドで指定した形式でデータを出力しています。

実際の出力結果は以下のようになります。

```Bash
Emily is 30 years old and works as an Accountant.
```

## 深堀する

CSVファイルを扱う場合、注意しなければならないことがあります。それは、データ内にコンマが含まれる場合です。コンマが含まれることで、CSVファイルのデータ構造が壊れてしまう可能性があります。そのため、データ内にコンマが含まれる場合は、ダブルクォートでデータを囲んでおく必要があります。

また、CSVファイルを扱う際には、列の数やデータの型についても注意が必要です。列の数が一致しない場合や、数値データが文字列として扱われてしまう場合があります。そのため、データを正しく解析するためには、適切なデータ型変換やエラーハンドリングが必要になります。

最後に、CSVファイルを扱う際には、エクセルなどのプログラムを使って編集する方法もありますが、Bashを使ってプログラムでデータを処理することで、より柔軟かつ効率的に作業を行うことができます。

## もっと詳しく知るには

- [Bashの基本コマンド