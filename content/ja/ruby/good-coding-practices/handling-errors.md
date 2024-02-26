---
date: 2024-01-26 00:57:21.132586-07:00
description: "\u30A8\u30E9\u30FC\u51E6\u7406\u3068\u306F\u3001\u4E88\u671F\u305B\u306C\
  \u30B3\u30FC\u30C9\u5185\u306E\u30A8\u30E9\u30FC\u3092\u4E0A\u624B\u304F\u6271\u3063\
  \u305F\u308A\u3001\u30AF\u30E9\u30C3\u30B7\u30E5\u305B\u305A\u306B\u554F\u984C\u3092\
  \u7BA1\u7406\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u6B63\u5E38\u306B\u52D5\u4F5C\u3057\u306A\u3044\u3068\u304D\u306B\
  \u30D5\u30ED\u30FC\u3092\u30B3\u30F3\u30C8\u30ED\u30FC\u30EB\u3057\u3001\u30E6\u30FC\
  \u30B6\u30FC\u4F53\u9A13\u3092\u6ED1\u3089\u304B\u306B\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.797201-07:00'
model: gpt-4-1106-preview
summary: "\u30A8\u30E9\u30FC\u51E6\u7406\u3068\u306F\u3001\u4E88\u671F\u305B\u306C\
  \u30B3\u30FC\u30C9\u5185\u306E\u30A8\u30E9\u30FC\u3092\u4E0A\u624B\u304F\u6271\u3063\
  \u305F\u308A\u3001\u30AF\u30E9\u30C3\u30B7\u30E5\u305B\u305A\u306B\u554F\u984C\u3092\
  \u7BA1\u7406\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u6B63\u5E38\u306B\u52D5\u4F5C\u3057\u306A\u3044\u3068\u304D\u306B\
  \u30D5\u30ED\u30FC\u3092\u30B3\u30F3\u30C8\u30ED\u30FC\u30EB\u3057\u3001\u30E6\u30FC\
  \u30B6\u30FC\u4F53\u9A13\u3092\u6ED1\u3089\u304B\u306B\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
---

{{< edit_this_page >}}

## 何となぜ？

エラー処理とは、予期せぬコード内のエラーを上手く扱ったり、クラッシュせずに問題を管理することです。プログラマーは、正常に動作しないときにフローをコントロールし、ユーザー体験を滑らかにするためにこれを行います。

## 方法：

Rubyでは`begin`、`rescue`、`ensure`、および`end`を用いてエラーを処理します。リスクのあるコードを`begin`と`end`で囲みます。エラーが発生した場合、`rescue`が機能します。

```Ruby
begin
  # リスクのあるコードをここに書く。
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "Oops! それはできません: #{e.message}"
ensure
  puts "これは常に実行されます、エラーがあるかないかに関わらず。"
end
```

サンプル出力：
```
Oops! それはできません: 0で除算しました
これは常に実行されます、エラーがあるかないかに関わらず。
```

## 詳細解説

歴史的に、プログラミング言語におけるエラー処理は大きく進化し、初期の言語には粗末であったり存在しないメカニズムもありました。Rubyの例外処理は、PythonやSmalltalkのような言語によって刺激を受けています。

Rubyにおける`begin-rescue`の代替法としては、メソッド定義内で`rescue`を使うか、標準的なエラー処理には使われないが、非標準のフロー制御のために`throw`と`catch`を使う方法があります。

興味深い詳細の一つに、Rubyの例外はオブジェクト（`Exception`クラスおよびその子孫のインスタンス）であることがあります。従って、カスタムエラークラスを定義し、単にエラーをログに記録する以上のことを行うことができます。これにより、より堅牢なエラー処理のためにプログラム内の豊かな状態を運ぶことができます。

## 関連情報

- Rubyの例外とエラー処理に関するドキュメント: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Rubyエラー処理に関するベストプラクティスの詳細なガイド: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
