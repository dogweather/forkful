---
title:                "Fish Shell: 「標準エラーへの書き込み」"
simple_title:         "「標準エラーへの書き込み」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ？
標準エラー出力を書き込むことに参加する理由を紹介します。

Fish Shellのプログラミングを楽しむために必要な知識について学びましょう。

## 方法
```Fish Shell
echo "エラーが発生しました" 1>&2
```
上記のコードを実行すると、エラーを標準エラー出力に書き込むことができます。これにより、プログラムがどのようなエラーを引き起こしたかを明確に表示することができます。

## 深く掘り下げる
標準エラー出力を使用することで、プログラムのデバッグがより簡単になります。また、パイプやリダイレクトを使用して、プログラムの出力を正しく処理することもできます。

## See Also
- [Fish Shellの公式ドキュメンテーション](https://fishshell.com/docs/current/)
- [標準エラー出力を使ったデバッグの方法](https://www.shellhacks.com/ja/redirect-stderr-stdout-file/)
- [プロフェッショナルなシェルスクリプト - デバッグ](https://www.shellscript.sh/debugging.html)