---
title:                "「csvファイルの操作」"
html_title:           "C++: 「csvファイルの操作」"
simple_title:         "「csvファイルの操作」"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## CSVって何？
CSVとはコンマで区切られたデータ形式のことです。プログラマーがCSVを使う理由は、簡単にデータを保存し、読み込み、処理できるからです。

## 方法：
### データをCSVファイルとして保存する
```C++
#include <fstream>
using namespace std;

int main() {
    ofstream csv_file; // CSVファイルを保存するためのファイルオブジェクトを作成
    csv_file.open("data.csv"); // 新しいCSVファイルを作成

    // データをコンマで区切り、ファイルに書き込む
    csv_file << "hoge, foo, bar" << endl;
    csv_file << "123, 456, 789" << endl;

    csv_file.close(); // ファイルを閉じる
    return 0; 
}
```

### CSVファイルからデータを読み込む
```C++
#include <iostream>
#include <fstream>
#include <string>
using namespace std;

int main() {
    ifstream csv_file; // CSVファイルを読み込むためのファイルオブジェクトを作成
    string line;
    csv_file.open("data.csv"); // 既存のCSVファイルを開く

    // 1行ずつデータを読み込み、コンマで区切って表示する
    while (getline(csv_file, line)) {
        cout << "データ: " << line << endl;
        // コンマで区切られた各要素を取り出す
        string data[3]; // 3つの要素があるCSVファイルを想定
        int i = 0;
        for (char& c: line) {
            if (c == ',') {
                i++;  
            } else {
                data[i] += c;
            }
        }

        // 取り出したデータを表示
        cout << "hoge: " << data[0] << endl;
        cout << "foo: " << data[1] << endl;
        cout << "bar: " << data[2] << endl;
    }

    csv_file.close(); // ファイルを閉じる
    return 0;
}
```

### データをCSV形式で出力する
```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    int hoge = 123;
    string foo = "abc";
    char bar = 'x';

    // 出力ファイルの準備
    ofstream csv_file;
    csv_file.open("output.csv");

    // データをコンマで区切り、ファイルに書き込む
    csv_file << hoge << "," << foo << "," << bar << endl;

    csv_file.close(); // ファイルを閉じる
    return 0;
}
```

## 深堀り：
### 歴史的背景
CSVは1972年にプログラマーのハル・バクステッドが発案し、汎用データ形式として広く使われるようになりました。当初は「Comma Separated Value」という略語で呼ばれていましたが、後に「Character Separated Values」や「Comma Delimited」とも呼ばれるようになりました。

### 代替手段
CSVの代替として、ExcelやMySQLなどのデータベースを使用する方法もあります。しかし、小規模なデータ処理ではまだまだCSVが有用です。

### 実装詳細
ファイルの入出力機能を実現するために、C++では```fstream```という標準ライブラリが用意されています。これを使用することで、ファイルを開いたり、読み書きすることができます。また、文字列の操作やデータの変換には、```string```や```stringstream```を使用することができます。

## 関連リンク：
- [CSVの歴史](https://en.wikipedia.org/wiki/Comma-separated_values#History)
- [ファイル入出力の詳細](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)

[ファイル入力と出力に関する参考記事](https://cpprefjp.github.io/reference/fstream.html)