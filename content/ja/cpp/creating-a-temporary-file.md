---
title:    "C++: 一時ファイルの作成"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ
一時ファイルを作成するのか、その理由について説明します。一時ファイルは、プログラムの実行中に一時的なデータを保存するために使われます。例えば、プログラムが大量のデータを処理する場合、一時ファイルを使用することでメモリを節約し、プログラムの実行を効率的に行うことができます。

## 作り方
一時ファイルを作成するには、C++の`<fstream>`ライブラリを使用します。以下の例では、`std::fstream`クラスの`open`関数を使って一時ファイルを作成し、データを書き込んでいます。

```C++
#include <fstream>

int main() {
  // ファイルを書き込みモードでオープン
  std::fstream temp_file("my_temp_file.txt", std::ios::out);

  // ファイルにデータを書き込む
  temp_file << "This is a temporary file" << std::endl;

  // ファイルを閉じる
  temp_file.close();
  
  return 0;
}
```

実行すると、プログラムがあるディレクトリに`my_temp_file.txt`という一時ファイルが作成され、"This is a temporary file"という内容が書き込まれます。

## 深掘り
一時ファイルを作成する際には、`std::ofstream`クラスではなく`std::fstream`クラスを使用することが推奨されています。`std::ofstream`では、ファイルが既に存在する場合にデータを上書きしてしまいますが、`std::fstream`では既存のファイルを上書きするのではなく、新しいファイルを作成するように制御することができます。

また、一時ファイルを作成する際には、プログラムが終了する際に一時ファイルを削除するようにすることが重要です。これにより、不要なファイルが残らず、システムのストレージが占有されることを防ぐことができます。

## また参照
- [C++ 公式ドキュメント - <fstream>](https://ja.cppreference.com/w/cpp/header/fstream)
- [C++ ファイル操作 - ファイルの入出力](https://programming-place.net/ppp/contents/cpp/file/006.html)