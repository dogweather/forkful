---
title:    "PHP: 文字列の大文字化"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## なぜキャピタライズする必要があるのか

プログラミングを行う際、時には文字列を大文字に変換する必要があります。例えば、ユーザーから入力された情報をデータベースに保存する前に、文字列を大文字に変換することで誤ったデータが入力されることを防ぐことができます。また、見栄えやデータの整合性を保つためにも、文字列を大文字に変換する必要があります。

## キャピタライズする方法

PHPでは、組み込みの関数`strtoupper()`を使用することで文字列を大文字に変換することができます。例えば、以下のように使用します。

```PHP
$name = "john smith";
echo strtoupper($name);
```

出力結果は`JOHN SMITH`となります。また、独自の関数を作成することでも文字列を大文字に変換することが可能です。以下は簡単な例です。

```PHP
function my_strtoupper($string) {
    $upper = "";
    for($i = 0; $i < strlen($string); $i++) {
        $char = $string[$i];
        if(ord($char) >= 97 && ord($char) <= 122) { //アルファベットの小文字の場合
            $char = chr(ord($char) - 32); //文字コードを差し引いて大文字に変換
        }
        $upper .= $char;
    }
    return $upper;
}

$name = "john smith";
echo my_strtoupper($name);
```

出力結果は同じく`JOHN SMITH`となります。

## キャピタライズについての詳細

大文字に変換する際、単純に文字コードを変換するだけではなく、多言語に対応するためのルールや例外処理などが含まれています。例えば、ドイツ語の`ß`を大文字に変換すると`SS`になるように仕様が定められています。また、アクセント付きの文字や記号なども適切に処理する必要があります。

さらに、文字列を大文字に変換する際、文字コードの変換にはある程度のコストがかかるため、大量の文字列を処理する場合にはパフォーマンスの面で注意が必要です。

## その他の記事

[PHPの組み込み関数:strtoupper()](https://www.php.net/manual/ja/function.strtoupper.php)
[ASCII文字コード表](http://www9.plala.or.jp/sgwr-t/c_sub/ascii.html)

# 参考リンク

- [PHPの組み込み関数:ord()](https://www.php.net/manual/ja/function.ord.php)
- [PHPの組み込み関数:chr()](https://www.php.net/manual/ja/function.chr.php)