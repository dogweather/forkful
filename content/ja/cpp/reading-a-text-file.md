---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何となぜ？ (What & Why?)

テキストファイルを読み込むとは、プログラムが情報を取得するためにファイルを開き、その内容を読み取ることです。これは、データを永続化し、後で再利用するために必要です。

## どのように: (How to)

以下にC++を用いてテキストファイルを読み取る基本的なコードを示します。

```C++
#include <fstream>
#include <iostream>
#include <string>
 
void readFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file) {
        std::cout << "File could not be opened!" << std::endl;
        return;
    }
 
    std::string line;
    while (std::getline(file, line)) {
        std::cout << line << std::endl;
    }
}
 
int main() {
    readFile("sample.txt");
    return 0;
}
```

これは"sample.txt"というファイルを一行ずつ読み込み、それぞれの行を出力します。

## 深海へ (Deep Dive)

1. **歴史的背景**: テキストファイルの読み取りは、コンピューティングの初期から存在しています。これはデータの永続化と情報の交換のための基本的な方法でした。

2. **代替方法**: バイナリファイルを読み込むでもデータを永続化できます。しかし、それらは人間が直接読むことは難しく、また一部のデータ構造はテキストファイルよりもバイナリファイル形式で保存した方が効率的です。

3. **実装詳細**: `std::ifstream`は、C++の入力ファイルストリームです。これは`std::ostream`を基底に持ち、テキストやバイナリファイルから読み取りを行うために使用します。

## 参考資料 (See Also)

1. C++ Reference, std::ifstream: http://www.cplusplus.com/reference/fstream/ifstream/
2. File I/O in C++: https://www.geeksforgeeks.org/file-handling-c-classes/