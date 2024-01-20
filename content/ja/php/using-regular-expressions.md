---
title:                "正規表現の使用"
html_title:           "PHP: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なに & なぜ?

正規表現を使うことは、テキストパターンを検索および置換する手段です。プログラマーは、データの整形やバリデーション、または文字列の処理において、この機能を使用します。

## 使い方:

```php
// 文字列中のメールアドレスの検索
$string = "Contact us at info@example.com";
$pattern = '/[\w.%+-]+@[\w.-]+\.[A-Za-z]{2,3}/';
preg_match($pattern, $string, $matches);
echo $matches[0]; // 出力: info@example.com
```

```php
// 正規表現を使った置換
$text = "Hello, my name is John.";
$pattern = '/John/';
$replacement = "Jane";
echo preg_replace($pattern, $replacement, $text); // 出力: Hello, my name is Jane.
```

```php
// パスワードのバリデーション
$password = "abc123$";
$pattern = '/^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[$@$!%*?&])[A-Za-z0-9$@$!%*?&]{8,}$/';
if (preg_match($pattern, $password)) {
  echo "Valid Password";
} else {
  echo "Invalid Password";
}
```

## 深堀り:

- 正規表現は、アメリカのコンピューターサイエンスで生まれましたが、現在では世界中のプログラミング言語で使用されています。
- PHPでは、preg_レギュラーエクスプレッション関数を使うことで正規表現を処理できますが、別の方法として、Perl互換正規表現(PCRE)を使う方法もあります。
- 正規表現は、パフォーマンスが重要なアプリケーションでは避けるべきです。処理するテキストが長くなると、正規表現は大変負荷がかかります。

## 関連リソース:

- [PHPで正規表現を使う方法](https://www.php.net/manual/en/book.pcre.php)
- [PHP正規表現チュートリアル](https://www.tutorialspoint.com/php/php_regular_expression.htm)