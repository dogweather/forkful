---
title:    "PHP: 「文字列の連結」"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## なぜ
文字列を連結することによって、より多くの情報を使いやすくまとめることができます。

## 方法
文字列を連結するには、PHP内で「.」を使います。例えば、以下のコードを使ってみましょう。

```PHP
$greeting = "こんにちは、";
$name = "太郎さん";
$full_greeting = $greeting . $name;

echo $full_greeting;
```

上記のコードで、「こんにちは、太郎さん」という出力が得られます。

## 深堀り
文字列を連結する際には、注意すべきポイントがいくつかあります。まず、連結する際には必ず間に「.」を入れる必要があります。また、文中にも文字列連結を使用することができます。例えば、以下のコードを見てみましょう。

```PHP
$message = "今日の天気は";
$weather = "晴れです。";

echo $message . $weather;
```

上記のコードでは、「今日の天気は晴れです。」という出力が得られます。また、変数内に特殊な文字を含む場合は、クォートで囲むこともできます。例えば、以下のコードを見てみましょう。

```PHP
$greeting = "こんにちは、";
$name = "太郎さん";

echo $greeting . "$nameさん。"; 
```

上記のコードでは、「こんにちは、太郎さん。」という出力が得られます。

## さらに見る
- [PHP Tutorial: Writing Your First PHP Script - Strings and Concatenation](https://www.learn-php.org/en/String_Functions_II#Concatenation)
- [Concatenation in PHP](https://www.w3schools.com/php/php_concatenation.asp)
- [The Dot Operator in PHP - Learn Two Ways to Combine Strings](https://www.lifewire.com/concatenation-dot-operator-818422) 

## その他
We hope this blog post was helpful in understanding how to concatenate strings in PHP. Happy coding!