---
title:    "PHP: 標準エラーへの書き込み"
keywords: ["PHP"]
---

{{< edit_this_page >}}

『なぜ書き込むの？』

標準エラー出力を書き込む理由は何でしょうか。プログラミングをする際に、標準エラーを出力に含めることで、エラーをより詳しく把握し、デバッグすることができます。

『やり方』

PHPで標準エラー出力を書き込む方法をご紹介します。下記のコードブロックにはコーディングの例と出力のサンプルが含まれています。
```
<?php
// 標準エラー出力を開く
$fp = fopen('php://stderr', 'w');
// エラーメッセージを書き込む
fwrite($fp, 'エラーが発生しました');
// ファイルを閉じる
fclose($fp);
```

出力：
```
エラーが発生しました
```

『深く掘り下げる』

標準エラー出力を書き込む方法には、いくつかのオプションがあります。例えば、エラーログをファイルに書き込む場合は「php://stderr」ではなく、指定したファイルのパスを使用します。また、エラーの種類や詳細な情報を追加することもできます。詳しくはPHPの公式ドキュメントを参照してください。

『関連リンク』

- PHP公式ドキュメント：https://www.php.net/manual/ja/function.fopen.php
- 標準エラー出力について知る：https://qiita.com/MoritaNaoki/items/600d678688611c1fd365