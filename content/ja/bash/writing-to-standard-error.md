---
title:                "標準エラーへの書き込み"
html_title:           "Bash: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 何ですか？：
標準エラー出力とは、プログラマーがコンピューターに特定のメッセージを表示するための方法です。例えば、エラーが発生した場合やデバッグ中に使用することができます。プログラマーがすぐに問題を特定し、修正するために重要なツールとなります。

# 方法：
```Bash
# エラーメッセージを標準エラー出力に表示する例
echo "エラーが発生しました" >&2

# ファイルが存在しない場合にエラーメッセージを表示する例
if [ ! -f "example.txt" ]; then
  echo "ファイルが存在しません" >&2
fi
```

実行結果：
```
エラーが発生しました
ファイルが存在しません
```

# 深堀り：
標準エラー出力は、プログラミング言語Bashの機能の一部であり、Unixシステムで最も一般的に使用されています。以前は、エラーメッセージを標準出力（通常のプログラム出力）に表示することが多かったため、プログラムの実行結果を誤って解釈することがありました。しかし、標準エラー出力を使用することで、プログラマーはエラーメッセージのみを見ることができ、プログラムの実行結果を見ることができます。標準エラー出力は標準出力と同様に「>」を使用してリダイレクトすることができます。

代替手段として、プログラマーはログファイルを使用することもできます。しかし、標準エラー出力を使用することで、プログラム実行中にログファイルを読む必要がなくなり、エラーを迅速に特定することができます。

# 関連情報：
- [標準エラー出力についてのBashドキュメント](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [標準エラー出力の活用方法](https://bash.cyberciti.biz/guide/The_bash_shell#Standard_Error_Redirection)