---
title:    "PHP: コンピュータープログラミングの記事タイトル：「コマンドライン引数の読み取り」"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ
コマンドライン引数を読み取ることの重要性について、1-2文で説明します。コマンドライン引数を使うことで、スクリプトを様々な環境で柔軟に使用することができます。例えば、ユーザーが動作をカスタマイズするためのオプションを追加することができます。

## 使い方
コマンドライン引数を読み取るには、PHPの`argv`という特別な配列を使用します。これにはPHPスクリプトが実行される際に渡された引数が格納されています。以下は簡単な例です。

```PHP
<?php
  $first_name = $argv[1];
  $last_name = $argv[2];
  echo "こんにちは、".$last_name." ".$first_name."さん！";
?>
```

実行コマンドは以下のようになります。

`php greeting.php 太郎 山田`

出力結果は、`こんにちは、山田 太郎さん！`となります。

## ディープダイブ
コマンドライン引数を読み取る際には、`$argv`のインデックスについて理解することが重要です。最初の引数は`$argv[0]`になり、2つ目以降の引数は順番に`$argv[1]`、`$argv[2]`となります。また、`$argv`には実行ファイルのパスも含まれていることに注意してください。

コマンドライン引数に対して行うエスケープや検証など、セキュリティに関わる作業も重要です。引数のデータ型や長さを確認し、信頼できる値であることを確認するようにしましょう。

## See Also (関連リンク)
- [PHPの公式マニュアル](https://www.php.net/manual/en/reserved.variables.argv.php)
- [コマンドライン引数の使い方 (Qiita)](https://qiita.com/ynaka01/items/134cc5b4066f0156f86f)
- [PHPでコマンドライン引数をパースする方法 (Codezine)](https://codezine.jp/article/detail/10337)