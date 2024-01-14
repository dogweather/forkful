---
title:                "C++: CSVとの作業"
simple_title:         "CSVとの作業"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを扱うのか？

CSVは日々の業務に欠かせない重要なデータ形式です。例えば、商品の在庫管理や顧客情報の保存に使用することができます。また、ExcelやGoogle Sheetsなどで簡単に編集することができるため、多くの企業で使用されています。

## どのようにすればCSVを扱えるのか？

CSVを扱うためには、C++言語によるプログラミングが必要です。以下のコードブロックに示す通り、CSVファイルを読み込んだり、データを編集したりすることができます。また、データの取得や出力方法を自由にカスタマイズすることもできます。

```C++
// CSVファイルを読み込む例
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace std;

int main(){
  vector<vector<string> > data; // 2次元vectorを定義
  ifstream file("data.csv");
  string line = "";
  while(getline(file, line)){ // ファイルの終端まで繰り返し
    vector<string> row; // 行ごとにデータを格納するvectorを定義
    string col = "";
    for(char c : line){ // 行データを1文字ずつ処理
      if(c == ','){ // カンマで列を区切り、行のvectorに追加
        row.push_back(col);
        col = "";
      }else{
        col += c;
      }
    }
    row.push_back(col); // 最後の列を追加
    data.push_back(row); // 行を2次元vectorに追加
  }
  for(int i=0;i<data.size();i++){
    for(int j=0;j<data[i].size();j++){
      cout << data[i][j] << " "; // データをスペースで区切って出力
    }
    cout << endl;
  }
}
```

上記のコードは、CSVファイルからデータを読み込み、スペースで区切って出力するものです。他にも、データの編集や新しいCSVファイルの作成など、様々な処理が可能です。

## 深い部分を掘り下げる

CSVは非常にシンプルなファイル形式ですが、中には扱いづらい部分もあります。例えば、データ内に改行やカンマが含まれる場合、正しくデータを取得することができなくなってしまいます。また、文字コードの違いやファイルの形式によってもデータの取り扱いが異なります。

そのため、CSVを扱う際には注意が必要です。正確なデータ処理を行うためには、さまざまなテクニックを学ぶ必要があります。また、ライブラリを使用することでより簡単にデータの処理ができる場合もあります。

## 関連記事

- [C++でCSVファイルを扱う方法](https://qiita.com/sta/items/9fb9b6ef7b0220078128)
- [文字コードの違いによるCSVファイルの扱い方](https://biz.play-guitar.jp/cpp/csv-file-encoding/)
- [GitHubで公開されているCSVライブラリ一覧](https://github.com/topics/csv-library)