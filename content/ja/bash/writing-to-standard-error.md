---
title:    "Bash: 「標準エラーに書き込む」"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ？

プログラミングを学ぶときに、標準エラー出力に書き込む方法を知ることは非常に重要です。デバッグのためにエラーメッセージを取得したり、プログラムの実行状況を把握するために必要になるからです。

## 方法

標準エラー出力に書き込むには、次のようにコードを記述します。

```Bash
echo "Error message" >&2
```

このように、`>&2`を追加することで、エラーメッセージを標準エラー出力に書き込むことができます。また、コマンドの実行結果を標準エラー出力に書き込むこともできます。

```Bash
ls -l file1 file2 2>&1 | grep -i 'Permission denied'
```

上記の例では、`ls`コマンドの実行結果が標準エラー出力に書き込まれ、そこから`Permission denied`というエラーメッセージを検索します。

## 深堀り

標準エラー出力に書き込むことで、プログラムの実行状況を把握することができます。また、エラーメッセージを出力することで、バグの原因を特定しやすくなります。さらに、リダイレクト演算子(`>&`や`|`)を使用することで、標準エラー出力を標準出力と同じように扱うことができます。

## 参考リンク

- [Bashの標準エラー出力について](https://www.shellscript.sh/errormessages.html)
- [リダイレクト演算子の使用方法](https://www.linuxjournal.com/article/7383)
- [Unixコマンドの標準入出力について](https://linux.academicinfo.net/unix-command-io.html)

## 参考

[Bashのリダイレクト演算子](https://www.tutorialspoint.com/unix/unix-io-redirections.htm)