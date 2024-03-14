---
date: 2024-01-26 03:41:00.042059-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.229078-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
---

{{< edit_this_page >}}

## なぜ & なにを?
PHPで文字列から引用符を取り除くとは、コードロジックやデータベースクエリを混乱させかねない、面倒なダブルクォート（`"`）またはシングルクォート（`'`）の文字を削除することを意味します。プログラマーは、文字列が安全に使用または保存されるように、入力データをクリーンアップまたはサニタイズするためにこれを行います。

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
