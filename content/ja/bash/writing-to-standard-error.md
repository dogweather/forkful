---
title:                "Bash: 標準エラーへの書き込み"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ
標準エラーに書き込むことについて、なぜそうするのかを説明します。

デバッグやトラブルシューティングの際に、標準エラーに出力を書き込むことで、プログラムの実行結果を把握することができます。これにより、エラーの原因やプログラムの実行状況をより詳しく確認することができます。

## 使い方
標準エラーに書き込む方法について、簡単なコーディング例と出力を表示します。

```Bash
#! /bin/bash
ls
echo "This is a test" >&2
```

上記のコードを実行すると、標準エラーに "This is a test" というメッセージが表示されます。エラーが発生した場合でも、標準出力ではなく標準エラーにメッセージが書き込まれることが確認できます。

## 深く掘り下げる
標準エラーへの書き込みは、プログラミングにおいて非常に重要です。特に、大規模なプログラムのデバッグや、ログファイルの作成に役立ちます。また、リダイレクトやパイプを使用することで、標準エラーの出力を他のプログラムに渡すことができます。

さらに、標準エラーにはパイプラインの迂回が可能な "2>" 演算子があります。これにより、標準エラーをファイルに書き込むことができます。例えば、以下のようにコマンドを実行すると、標準エラーの出力を "error.log" ファイルに保存することができます。

```Bash
command 2> error.log
```

これにより、実行中のエラーを後で確認することができます。

## ぜひ参考にしてください
- [Linuxコマンドラインの標準出力と標準エラーについて](https://techacademy.jp/magazine/8180)
- [Bashの基礎知識～標準出力と標準エラーの使い分けについて～](https://qiita.com/t_nakayama0714/items/e0bbf6dac1ba29cf5f2e)
- [シェルスクリプトβ: 5.3. リダイレクション](https://shellscript.sunone.me/redirection.html#stderr)
- [リダイレクションとパイプ](https://www.atmarkit.co.jp/ait/articles/1405/22/news041.html)

# 参考

## 関連リンク
- [Markdown入門](https://www.markdown.jp/)
- [Bash入門](https://qiita.com/RyochanUedasan/items/53a9070a708ead53e529)
- [演算子一覧](https://www.atmarkit.co.jp/ait/articles/1410/31/news072.html)