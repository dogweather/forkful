---
title:    "C: 一時ファイルの作成"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

「なぜ」
一時的なファイルを作成することに、なぜあなたは興味を持つべきか？その答えは単純です。一時的なファイルは、プログラムの実行中にデータや情報を一時的に保存するために使用されます。例えば、大きなファイルを処理する際に一時ファイルを作成し、プログラムが終了したら削除することで、プログラムの実行速度を向上させることができます。

「作り方」
以下に示すコード例を参考にして、一時的なファイルを作成する方法を学びましょう。

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
  // 一時的なファイルを作成する
  FILE* temp = tmpfile();

  // 一時ファイルが正しく作成されたかを確認
  if (temp == NULL)
  {
    printf("一時的なファイルを作成できませんでした。\n");
    return 1;
  }

  // 一時ファイルにデータを書き込む
  fprintf(temp, "これは一時ファイルに書き込まれるデータです。\n");

  // 一時ファイルからデータを読み取り、出力する
  char buffer[256];
  rewind(temp); // ファイルポインタを先頭に戻す
  while (fgets(buffer, 255, temp) != NULL)
  {
    printf("%s", buffer);
  }

  // 一時ファイルを削除する
  fclose(temp);

  return 0;
}
```

実行結果は以下のようになります。

```C
これは一時ファイルに書き込まれるデータです。
```

「詳細」
一時的なファイルは、プログラムが実行されるデバイスの一時的な領域に作成されるため、常に存在するとは限りません。また、一時ファイルは通常、メインメモリを使用するため、ディスクへのアクセスが必要なファイルよりも高速に読み書きすることができます。しかしながら、一時ファイルはプロセスが終了すると自動的に削除されるため、プログラムが一時ファイルを使用する間に予期せずに削除されることがあります。そのため、必要なデータを書き込んだ後は、必ず一時ファイルを閉じて削除するようにしましょう。

「参考リンク」
- [C言語のtmpfile関数の仕様](http://www9.plala.or.jp/sgwr-t/lib/tmpfile.html)
- [一時ファイルの利用方法の例](https://qiita.com/shuujii/items/7c1ecf0191c2698ac922)
- [C言語入門：ファイル処理](https://www.cc.kyoto-su.ac.jp/~yamada/ap/2011/file.html)

「参考文献」
[一時的なファイル](https://en.wikipedia.org/wiki/Temporary_file)