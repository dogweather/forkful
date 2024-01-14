---
title:                "C++: 標準エラーへの書き込み"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ書くのか

スタンダードエラーに書き込むことは、デバッグやエラートラッキングなどの重要なプログラミングタスクを行う上で非常に役立ちます。この記事では、C++でスタンダードエラーに書き込む方法を説明します。

## 方法

C++では、標準出力と同様に、標準エラーに書き込むことができます。例えば、次のようにコーディングします。

```C++
#include <iostream>

using namespace std;

int main() {
  cerr << "これはスタンダードエラーに書き込まれます。" << endl;
  return 0;
}
```

上記のコードを実行すると、スタンダードエラーにメッセージが出力されます。例えば、ターミナル上で実行すると、次のように表示されます。

```
これはスタンダードエラーに書き込まれます。
```

このように、`cerr`オブジェクトを使用することで、スタンダードエラーに効率的に書き込むことができます。

## ディープダイブ

スタンダードエラーに書き込むことで、プログラムの実行中に起きたエラーを確認することができます。これは、デバッグやエラートラッキングなどの重要なタスクを行う際に特に有用です。また、標準エラーに書き込むことで、プログラムの実行速度が低下することもありません。

しかし、スタンダードエラーに書き込む際には注意点もあります。例えば、プログラムの実行結果をファイルにリダイレクトする場合、スタンダードエラーに書き込まれたメッセージも一緒にファイルに書き込まれることになります。そのため、必要に応じてメッセージのフィルタリングなどを行う必要があるかもしれません。

## 参考リンク

[標準エラーの出力方法（スタンダードエラーへの書き込み）](https://www.petitmonte.com/programming/stdcerr.html)

[C++入門 基本文法 第2部 1-7 出力ストリームから文字を取り出す](https://programming.pc-note.net/ccpp/output.html)

## 参考

[方法](https://docs.microsoft.com/en-us/cpp/standard-library/output-and-input-streams?view=vs-2019)

[ディープダイブ](https://www.tutorialspoint.com/cplusplus-program-to-write-a-statement-on-error-stream-using-std-cerr)