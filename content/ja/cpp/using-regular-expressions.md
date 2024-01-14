---
title:    "C++: 正規表現の使用"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ使うのか

正規表現を使う利点を説明します。正規表現を使うことで、テキストデータを簡単に検索・置換できるようになります。これはプログラミングにおいて非常に便利な機能です。

## 使い方

正規表現を使うには、まず ```#include <regex>``` というヘッダーファイルを追加する必要があります。次に、以下のようにコードを書くことで正規表現オブジェクトを作成することができます。

```
#include <regex>
 
std::regex regex_obj("正規表現パターン");
```

正規表現パターンの書き方は様々ありますが、基本的なものは以下の通りです。

- ```[0-9]```：数字を表します。
- ```[a-z]```：小文字のアルファベットを表します。
- ```[A-Z]```：大文字のアルファベットを表します。
- ```.*```：任意の文字列を表します。

例えば、「apple」という文字列を含む行を検索する正規表現パターンは以下のようになります。

```
^.*apple.*$
```

この正規表現パターンを使って、テキストファイルから「apple」が含まれる行を検索するコードは以下のようになります。

```
#include <iostream>
#include <fstream>
#include <regex>
 
using namespace std;
 
int main() {
    // ファイルを開く
    ifstream file("sample.txt");
 
    // ファイルが開けなかった場合はエラーを表示
    if (!file) {
        cout << "ファイルが開けませんでした。" << endl;
        return 0;
    }
 
    // 正規表現オブジェクトを作成
    regex regex_obj("^.*apple.*$");
 
    // 行ごとにテキストを読み込み、正規表現にマッチするかチェック
    string line;
    while (getline(file, line)) {
        if (regex_match(line, regex_obj)) {
            cout << line << endl;
        }
    }
 
    // ファイルを閉じる
    file.close();
 
    return 0;
}
```

以上のコードを実行すると、テキストファイルから「apple」が含まれる行が出力されます。

## 深堀り

正規表現にはより高度な機能もあります。例えば、置換を行うこともできます。```std::regex_replace()``` 関数を使うことで、特定の文字列を置換することができます。

また、正規表現パターンの中でグループを作成することもできます。これを使うことで、正規表現にマッチする部分文字列を取得することができます。

正規表現を使う際は、[正規表現チュートリアル](https://www.javadrive.jp/cpp/regex/)や[cppreference.com](https://en.cppreference.com/w/cpp/regex)などのサイトを参考にするとより詳細な情報を得ることができます。

## それではまた次回

以上で正規表現を使う方法を紹介しました。次回は正規表現を使った実践的な例を紹介します。お楽しみにしていてください。

## 関連情報

- [正規表現: cppreference.com](