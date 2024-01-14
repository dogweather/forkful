---
title:    "Fish Shell: コンピュータープログラミングにおける「コマンドライン引数の読み方」"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜ
コマンドライン引数の読み取りを学ぶ理由を説明します。コマンドライン引数は、プログラムが実行される際に与えられた追加の情報を提供するため、プログラムをより柔軟に作成することができます。

## 方法
コマンドライン引数を読み取る方法はいくつかありますが、Fish Shellを使用すると簡単に実装することができます。以下のようにコードを書くことで、コマンドライン引数を読み取ることができます。

```
Fish Shell

# コマンドライン引数を読み取る関数を定義
function read_arguments
  echo "引数1: " $argv[1]
  echo "引数2: " $argv[2]
  echo "引数3: " $argv[3]
end

# 関数を呼び出す
read_arguments foo bar baz
```

上記のコードを実行すると、以下のような結果が得られます。

```
引数1: foo
引数2: bar
引数3: baz
```

## 深いダイブ
コマンドライン引数を読み取る際に注意するべき点がいくつかあります。まず、コマンドライン引数の数に制限はありませんが、必ずしもすべてが必要なわけではありません。また、スペースを含む引数を正しく読み取るためには、クォーテーション（"）を使用する必要があります。さらに、Fish Shellでは、コマンドライン引数のほかにも環境変数や標準入力などの入力元を使用することができます。

## また見る
- [Fish Shellの公式ドキュメント](https://fishshell.com/docs/current/index.html)
- [コマンドライン引数の取得方法について](https://qiita.com/tmknom/items/e485421516fc7dd5013b)
- [環境変数とコマンドライン引数の違いについて](https://www.labri.fr/perso/betrema/debian/21036-ca.pdf)