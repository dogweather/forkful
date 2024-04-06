---
date: 2024-01-26 03:41:00.042059-07:00
description: ''
lastmod: '2024-04-05T22:50:56.147709-06:00'
model: gpt-4-0125-preview
summary: "PHP\u306E\u521D\u671F\u306E\u3053\u308D\u3001\u958B\u767A\u8005\u306F\u7279\
  \u306B\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\u306B\u30C7\u30FC\u30BF\u3092\u633F\u5165\
  \u3059\u308B\u969B\u306B\u6587\u5B57\u5217\u306E\u5F15\u7528\u7B26\u306B\u6CE8\u610F\
  \u3092\u6255\u308F\u306A\u3051\u308C\u3070\u306A\u308A\u307E\u305B\u3093\u3067\u3057\
  \u305F\u3002\u4E0D\u9069\u5207\u306B\u6271\u308F\u308C\u305F\u5F15\u7528\u7B26\u306F\
  SQL\u30A4\u30F3\u30B8\u30A7\u30AF\u30B7\u30E7\u30F3\u653B\u6483\u306B\u3064\u306A\
  \u304C\u308B\u53EF\u80FD\u6027\u304C\u3042\u308A\u307E\u3057\u305F\u3002\u3053\u3053\
  \u3067\u3001\u5165\u529B\u30C7\u30FC\u30BF\u3092\u81EA\u52D5\u7684\u306B\u30A8\u30B9\
  \u30B1\u30FC\u30D7\u3059\u308B\u6A5F\u80FD\u3067\u3042\u308B\u30DE\u30B8\u30C3\u30AF\
  \u30AF\u30A9\u30FC\u30C8\u304C\u767B\u5834\u3057\u307E\u3059\u3002\u3053\u308C\u306F\
  \u975E\u63A8\u5968\u3068\u306A\u308A\u3001\u6700\u7D42\u7684\u306B\u306F\u60AA\u3044\
  \u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\u6163\u884C\u3068\u30BB\u30AD\u30E5\u30EA\u30C6\
  \u30A3\u554F\u984C\u3092\u52A9\u9577\u3059\u308B\u305F\u3081\u524A\u9664\u3055\u308C\
  \u307E\u3057\u305F\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 方法:
以下は、PHPの組み込み関数を使用した簡単な例です：

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // 出力: Hello, she said, Its a fine day!
```

簡単ですね？この`str_replace()`関数は、文字列から削除する文字の配列を取ります。これには、シングルクォートとダブルクォートの両方が含まれます。

## 深堀り
PHPの初期のころ、開発者は特にデータベースにデータを挿入する際に文字列の引用符に注意を払わなければなりませんでした。不適切に扱われた引用符はSQLインジェクション攻撃につながる可能性がありました。ここで、入力データを自動的にエスケープする機能であるマジッククォートが登場します。これは非推奨となり、最終的には悪いコーディング慣行とセキュリティ問題を助長するため削除されました。

現在では、`str_replace()`やより高度なパターンには`preg_replace()`と正規表現を使用します。以下は正規表現の例です：

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

JSONデータの場合、余分なバックスラッシュを引用符に含めないように、`JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE`のようなオプションと共に`json_encode()`を使用するかもしれません。

実装する際は、エッジケースを考慮してください。あなたの文字列が、物語の対話や測定のインチのように、特定の引用符を含むことを意図している場合はどうでしょうか？文脈が重要ですので、データの意図された使用に合わせて引用符の削除を調整してください。

## 参照
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: SQLインジェクション防止](https://owasp.org/www-community/attacks/SQL_Injection)
