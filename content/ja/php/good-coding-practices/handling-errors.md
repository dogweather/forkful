---
date: 2024-01-26 00:56:23.465690-07:00
description: "PHP\u3067\u306E\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u306E\u901A\u5E38\u306E\u6D41\u308C\u3092\u7834\u308B\u6761\u4EF6\
  \u3001\u305F\u3068\u3048\u3070\u30D5\u30A1\u30A4\u30EB\u304C\u898B\u3064\u304B\u3089\
  \u306A\u3044\u3001\u30C7\u30FC\u30BF\u5165\u529B\u304C\u4E0D\u6B63\u306A\u3069\u306B\
  \u7BA1\u7406\u3068\u5BFE\u5FDC\u3092\u884C\u3046\u3053\u3068\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30A8\u30E9\u30FC\u3092\u51E6\u7406\u3057\u3066\
  \u30AF\u30E9\u30C3\u30B7\u30E5\u3092\u9632\u304E\u3001\u30E6\u30FC\u30B6\u30FC\u306B\
  \u30B9\u30E0\u30FC\u30BA\u306A\u4F53\u9A13\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.255405-06:00'
model: gpt-4-1106-preview
summary: "PHP\u3067\u306E\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306E\u901A\u5E38\u306E\u6D41\u308C\u3092\u7834\u308B\u6761\u4EF6\u3001\
  \u305F\u3068\u3048\u3070\u30D5\u30A1\u30A4\u30EB\u304C\u898B\u3064\u304B\u3089\u306A\
  \u3044\u3001\u30C7\u30FC\u30BF\u5165\u529B\u304C\u4E0D\u6B63\u306A\u3069\u306B\u7BA1\
  \u7406\u3068\u5BFE\u5FDC\u3092\u884C\u3046\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u30A8\u30E9\u30FC\u3092\u51E6\u7406\u3057\u3066\u30AF\
  \u30E9\u30C3\u30B7\u30E5\u3092\u9632\u304E\u3001\u30E6\u30FC\u30B6\u30FC\u306B\u30B9\
  \u30E0\u30FC\u30BA\u306A\u4F53\u9A13\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002"
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
---

{{< edit_this_page >}}

## 何となぜ？
PHPでのエラー処理は、プログラムの通常の流れを破る条件、たとえばファイルが見つからない、データ入力が不正などに管理と対応を行うことです。プログラマーはエラーを処理してクラッシュを防ぎ、ユーザーにスムーズな体験を提供します。

## 方法：
PHPでは、`try-catch` ブロックを使ってエラーを管理し、カスタムエラーハンドラーや例外を使って処理をカスタマイズすることができます。

```php
// 基本的なtry-catchの例
try {
  // リスクのある何かを行う
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // エラーを処理する
  echo "Error: " . $e->getMessage();
}

// カスタムエラーハンドラーの設定
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// 例外の使用
class MyException extends Exception {}

try {
  // 何かを行いカスタム例外を投げる
  throw new MyException("Custom error!");
} catch (MyException $e) {
  // カスタム例外を処理する
  echo $e->getMessage();
}

// 出力サンプル：
// Error: fopen(nonexistentfile.txt): failed to open stream: No such file or directory
// Custom error!
```

## 深掘り
昔、PHPのエラーはスクリプトの実行を停止しない警告や通知に関するものが多かったです。言語が成熟するにつれて、PHP 5で導入されたExceptionクラスによるより堅牢なオブジェクト指向エラー処理が採用されました。その後、PHP 7はエラーと例外を区別するErrorクラスを導入しました。

`try-catch` ブロックの前には、PHPは `set_error_handler()` を使用してエラーに対処していました。`try-catch` はよりクリーンでモダンです。しかし、カスタムエラーハンドラーもまだ役割を持っており、特にレガシーコードや通常は例外エラーとされないものをキャッチする必要がある場合に便利です。

PHP 7+の `Throwable` インターフェースは、エラーであろうと例外であろうと、両方をキャッチできることを意味します。これは便利です。なぜなら、以前は追跡が難しかった重大なランタイムエラーを逃さないからです。

PHPの組み込みメカニズムの外部の代替手段には、ファイルへのエラーログの記録やユーザーフレンドリーなエラーページの表示など、より多くの機能を提供するライブラリやフレームワークが含まれています。

## 参照
- 公式PHPドキュメント上の例外：https://www.php.net/manual/ja/language.exceptions.php
- PHP The Right Wayのエラー報告について：https://phptherightway.com/#error_reporting
- PHPマニュアルのエラー処理について：https://www.php.net/manual/ja/book.errorfunc.php
