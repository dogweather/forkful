---
date: 2024-01-26 00:56:23.465690-07:00
description: "\u65B9\u6CD5\uFF1A PHP\u3067\u306F\u3001`try-catch` \u30D6\u30ED\u30C3\
  \u30AF\u3092\u4F7F\u3063\u3066\u30A8\u30E9\u30FC\u3092\u7BA1\u7406\u3057\u3001\u30AB\
  \u30B9\u30BF\u30E0\u30A8\u30E9\u30FC\u30CF\u30F3\u30C9\u30E9\u30FC\u3084\u4F8B\u5916\
  \u3092\u4F7F\u3063\u3066\u51E6\u7406\u3092\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u3059\
  \u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:43.108303-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

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
