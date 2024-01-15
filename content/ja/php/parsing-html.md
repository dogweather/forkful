---
title:                "「HTMLの解析」"
html_title:           "PHP: 「HTMLの解析」"
simple_title:         "「HTMLの解析」"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ

HTMLを解析する理由は、ウェブサイトから必要な情報を抽出するためです。たとえば、ウェブスクレイピングやデータ収集を行う際に、HTMLをパースして必要なデータを取得したり、不要な情報を除外したりする必要があります。

## 方法

HTMLをパースする最も簡単な方法は、PHPの組み込み関数である`file_get_contents()`を使用することです。この関数を使用すると、ウェブページからHTMLを取得することができます。例えば、以下のように記述します。

```PHP
$html = file_get_contents("https://example.com");
```

これにより、`$html`変数にはHTMLのコードが文字列として格納されます。次に、このHTMLコードをパースするために、PHPの組み込み関数である`preg_match()`を使用します。この関数を使用すると、指定したパターンにマッチする部分を抽出することができます。例えば、以下のように記述します。

```PHP
preg_match("/<title>(.*)<\/title>/", $html, $matches);

echo "ウェブサイトのタイトルは" . $matches[1] . "です。";
```

上記のコードでは、正規表現を使用してHTMLコードから`<title>`タグの中身を抽出し、`$matches`配列に格納しています。そして、`echo`文を使用してタイトルを表示しています。

## ディープダイブ

HTMLをパースする際に重要な点は、正規表現を使用することです。正規表現を使用することで、特定のパターンを指定してHTMLコードから必要な部分を抽出することができます。また、`preg_match()`以外にも、`preg_match_all()`や`preg_replace()`などの組み込み関数を使用することで、より高度なHTMLのパースが可能です。

## 関連リンク

- [PHP公式ドキュメント](https://www.php.net/manual/ja/function.file-get-contents.php)
- [正規表現チュートリアル](https://www.geeksforgeeks.org/php-programming-language/?ref=lbp)
- [ウェブスクレイピングの方法](https://www.digitalocean.com/community/tutorials/how-to-scrape-a-website-using-php-and-save-the-data-to-a-file)