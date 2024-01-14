---
title:                "C++: テキストファイルの読み込み"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読むのに参加する理由は様々ですが、主なものは以下の通りです。

- テキストファイルに格納されているデータを解析したいとき
- テキストファイルから情報を取得して別のアプリケーションで使用するため

どのような目的であっても、C++言語によるテキストファイルの読み込みは非常に重要なスキルです。

## 使い方

まず、テキストファイルを開くためにはiostreamライブラリを使用する必要があります。以下のコードを使用してテキストファイルを開きます。

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    ifstream file("myfile.txt"); // myfile.txtのファイル名を適宜変更
    string line;

    if (file.is_open()) {
        while (getline(file, line)) {
            cout << line << endl;
        }
        file.close();
    }
    return 0;
}
```

上記のコードでは、`ifstream`クラスを使ってファイルを開き、`getline()`関数を使用して1行ずつテキストを読み込んでいます。そして、読み込んだテキストを`cout`を使って出力しています。最後に、ファイルを閉じるために`file.close()`を使用します。

## 深堀り

以上の例では、読み込んだテキストを文字列として取得しました。しかし、テキストファイルから整数や浮動小数点数などの他のデータ型を取得する必要がある場合もあります。その場合、`ifstream`クラスの`>>`演算子を使用することで、異なるデータ型を読み込むことができます。

```C++
int num;
file >> num; // テキストファイルから整数を読み込む
```

また、テキストファイルに書き込むこともできます。その場合は`ofstream`クラスを使用します。例えば、`myfile.txt`というファイルに数字を書き込むコードは以下のようになります。

```C++
ofstream outfile ("myfile.txt");
int num = 10;
outfile << num; // テキストファイルに数値を書き込む
outfile.close();
```

これらの使い方を実践しながら、自分で様々な読み込みや書き込みの方法を試してみることをお勧めします。

## 参考リンク

- [C++でテキストファイルを読み込む方法](https://www.youtube.com/watch?v=mMUh56KWMw0)
- [C++ iostreamライブラリのドキュメント](https://en.cppreference.com/w/cpp/io)