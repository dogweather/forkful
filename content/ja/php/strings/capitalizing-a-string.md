---
title:                "文字列を大文字にする"
aliases:
- /ja/php/capitalizing-a-string/
date:                  2024-02-03T19:06:04.955400-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
文字列の先頭を大文字にすることで、データセット内で文、タイトル、または固有名詞が正しく始まるようにする操作を指します。プログラマーは、データの正規化、可読性の向上、またはユーザー入力やテキストデータ処理の一貫性を確保するために、文字列の先頭文字を大文字にする操作を頻繁に実行します。

## 方法：
PHPは標準で、文字列を大文字にするための様々な関数をサポートしており、それぞれ異なる目的に役立ちます。ここではその使用方法を紹介します：

### 文字列の最初の文字を大文字にする:

```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // 出力: Hello, world!
```

### 各単語の最初の文字を大文字にする:

```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // 出力: Hello, World!
```

### 文字列全体を大文字に変換する:

```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // 出力: HELLO, WORLD!
```

よりカスタマイズが必要な場合やサードパーティの解決策を利用する場合には、特に基本のASCIIセットを越える文字を扱う国際化の際に、`mbstring`（マルチバイト文字列用）のようなライブラリを利用できます。

### mbstringを使用してUTF-8文字列を大文字にする:

PHP設定で`mbstring`拡張機能が有効になっていることを確認してから：

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // 出力: Élégant
```

このアプローチにより、ASCII以外の文字を含む文字列を、様々な言語のニュアンスに従って正確に大文字にすることができます。
