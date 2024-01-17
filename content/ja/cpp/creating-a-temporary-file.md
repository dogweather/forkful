---
title:                "一時ファイルの作成"
html_title:           "C++: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 作成するのは何? & なぜ?: 
一時ファイルを作成するとは, プログラマーが作成する一時的なファイルのことです. プログラマーが一時ファイルを作成するのは, ファイルに一時的なデータを保存するため, あるいはコードの実行中に一時的な結果を保存するためです.

## 作り方: 
次のコードブロックには, ```C++ ...```タグで囲まれた例とサンプルのコードが含まれています.

```C++
#include <iostream>
#include <fstream>

int main() {
  // 一時ファイルを作成する
  std::ofstream tempFile("temporary.txt");
  tempFile << "これは一時ファイルです." << std::endl;

  // ファイルを読み込む
  std::ifstream readFile("temporary.txt");
  std::string line;
  while (std::getline(readFile, line)) {
    // 一時ファイルの内容を表示する
    std::cout << line << std::endl;
  }

  // 一時ファイルを削除する
  std::remove("temporary.txt");

  return 0;
}
```

上記のコードでは, ```std::ofstream```を使用して一時ファイルを作成し, その内容を書き込んでいます. そして, ```std::ifstream```を使用して一時ファイルの内容を読み込み, 最後に```std::remove```を使用して一時ファイルを削除しています.

## 詳細を深く掘り下げる: 
一時ファイルを作成する機能は, C言語の標準ライブラリで定義されており, C++でも使用することができます. また, プログラムの実行中に一時ファイルを作成する代わりに, ランダムな名前のファイルを作成してデータを保存することもできます. 一時ファイルは通常, プログラムが終了した時点で自動的に削除されますが, 必要に応じて手動で削除することもできます.

## 関連情報: 
- [C++で一時ファイルを作成する方法](https://qiita.com/hikalium/items/f1fd0cf68f4487eef9b3)
- [C言語の標準ライブラリについて](https://www.ibm.com/support/knowledgecenter/ja/ssw_ibm_i_74/rtref/createtempfile.htm)